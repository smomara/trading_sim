use reqwest::blocking::Client;
use serde::{Deserialize, Serialize};
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use chrono::{TimeZone, Utc};

#[derive(Deserialize, Debug)]
struct PolygonResponse {
    ticker: String,
    results: Vec<DailyData>,
    status: String,
}

#[derive(Deserialize, Debug)]
struct DailyData {
    c: f64,
    h: f64,
    l: f64,
    o: f64,
    t: i64,
    v: f64,
}

#[derive(Serialize, Debug)]
pub struct StockData {
    date: String,
    open: f64,
    high: f64,
    low: f64,
    close: f64,
    volume: f64,
}

#[no_mangle]
pub extern "C" fn fetch_stock_data(
    symbol: *const c_char,
    start_date: *const c_char,
    end_date: *const c_char,
    api_key: *const c_char,
) -> *mut c_char {
    let symbol = unsafe { CStr::from_ptr(symbol).to_str().unwrap() };
    let start_date = unsafe { CStr::from_ptr(start_date).to_str().unwrap() };
    let end_date = unsafe { CStr::from_ptr(end_date).to_str().unwrap() };
    let api_key = unsafe { CStr::from_ptr(api_key).to_str().unwrap() };

    let url = format!(
        "https://api.polygon.io/v2/aggs/ticker/{}/range/1/day/{}/{}?adjusted=true&sort=asc&apiKey={}",
        symbol, start_date, end_date, api_key
    );

    let client = Client::new();
    let response = match client.get(&url).send() {
        Ok(resp) => resp,
        Err(e) => {
            let error_msg = format!("HTTP request failed: {}", e);
            return CString::new(error_msg).unwrap().into_raw();
        }
    };

    let response_text = match response.text() {
        Ok(text) => text,
        Err(e) => {
            let error_msg = format!("Failed to get response text: {}", e);
            return CString::new(error_msg).unwrap().into_raw();
        }
    };

    let polygon_data: PolygonResponse = match serde_json::from_str(&response_text) {
        Ok(data) => data,
        Err(e) => {
            let error_msg = format!("Failed to parse JSON: {}", e);
            return CString::new(error_msg).unwrap().into_raw();
        }
    };

    if polygon_data.status != "OK" {
        let error_msg = format!("API returned non-OK status: {}", polygon_data.status);
        return CString::new(error_msg).unwrap().into_raw();
    }

    let stock_data: Vec<StockData> = polygon_data.results
        .into_iter()
        .map(|data| {
            let date = Utc.timestamp_millis_opt(data.t)
                .unwrap()
                .naive_utc()
                .date()
                .format("%Y-%m-%d")
                .to_string();
            StockData {
                date,
                open: data.o,
                high: data.h,
                low: data.l,
                close: data.c,
                volume: data.v,
            }
        })
        .collect();

    let json = match serde_json::to_string(&stock_data) {
        Ok(json) => json,
        Err(e) => {
            let error_msg = format!("Failed to serialize stock data to JSON: {}", e);
            return CString::new(error_msg).unwrap().into_raw();
        }
    };

    CString::new(json).unwrap().into_raw()
}

#[no_mangle]
pub extern "C" fn free_string(s: *mut c_char) {
    unsafe {
        if s.is_null() { return }
        CString::from_raw(s)
    };
}

