from flask import Flask, render_template, jsonify, request
import os
import json
from datetime import datetime
import requests
from concurrent.futures import ThreadPoolExecutor

app = Flask(__name__)
# No longer strictly relying on local DATA_DIR for visualization
REPO_OWNER = "roger-infanger-weibel"
REPO_NAME = "luzern-parking-monitoring"
GITHUB_API_BASE = f"https://api.github.com/repos/{REPO_OWNER}/{REPO_NAME}/contents/data"

@app.route('/')
def index():
    return render_template('index.html')

def fetch_file_content(url):
    try:
        resp = requests.get(url)
        if resp.status_code == 200:
            return resp.json() 
            # Note: GitHub raw content might be text, but since we are fetching download_url 
            # from the directory listing which points to raw user content, requests.json() works if file is json.
    except Exception as e:
        print(f"Error fetching {url}: {e}")
    return None

@app.route('/api/data')
def get_data():
    date_str = request.args.get('date')
    if not date_str:
        date_str = datetime.now().strftime("%Y-%m-%d")

    # 1. Get list of files for the date from GitHub API
    api_url = f"{GITHUB_API_BASE}/{date_str}"
    print(f"Fetching file list from: {api_url}")
    
    try:
        response = requests.get(api_url)
        if response.status_code == 404:
             return jsonify({"error": "No data found for this date (on GitHub)", "datasets": []})
        response.raise_for_status()
        files = response.json()
    except requests.RequestException as e:
        return jsonify({"error": f"GitHub API Error: {str(e)}", "datasets": []})

    # Filter for .json files and get their download URLs
    # GitHub API returns a list of dictionaries with 'name', 'download_url', etc.
    json_file_urls = [f['download_url'] for f in files if f['name'].endswith('.json')]
    
    timeline = []
    all_parking_ids = set()
    parking_details = {} 

    # 2. Fetch all files in parallel
    print(f"Found {len(json_file_urls)} files. Fetching content...")
    
    with ThreadPoolExecutor(max_workers=10) as executor:
        results = executor.map(fetch_file_content, json_file_urls)
        
    for content in results:
        if not content:
            continue
            
        try:
             # Extract time from 'time' field in json or assume it came from filename (not passed here easily without tuple map)
             # The JSON content has "time": "2026-01-02T08:06:57+01:00"
             # Let's parse that.
             
            timestamp_str = content.get("data", {}).get("time")
            if not timestamp_str:
                 continue
                 
            # Format time for display (HH:MM)
            dt = datetime.fromisoformat(timestamp_str)
            time_part = dt.strftime("%H:%M")
             
            snapshot = {
                "time": time_part,
                "data": {},
                "original_dt": dt # Store for sorting
            }

            if "data" in content and "parkings" in content["data"]:
                parkings = content["data"]["parkings"]
                for pid, pdata in parkings.items():
                    vacancy = pdata.get("vacancy", 0)
                    snapshot["data"][pid] = vacancy
                    all_parking_ids.add(pid)
                    if pid not in parking_details:
                        parking_details[pid] = pdata.get("description", pid)
            
            timeline.append(snapshot)
        except Exception as e:
            print(f"Error parsing content: {e}")

    # Sort timeline by time
    timeline.sort(key=lambda x: x["original_dt"])

    # Format for Chart.js
    labels = [t["time"] for t in timeline]
    datasets = []
    
    for pid in sorted(list(all_parking_ids)):
        data_points = []
        for point in timeline:
            data_points.append(point["data"].get(pid, None))
            
        dataset = {
            "label": parking_details.get(pid, pid),
            "data": data_points,
            "fill": False,
            "tension": 0.1
        }
        datasets.append(dataset)

    return jsonify({
        "labels": labels,
        "datasets": datasets,
        "date": date_str
    })

@app.route('/api/dates')
def get_dates():
    # List contents of the 'data' directory in the repo
    # Expecting a list of directories named YYYY-MM-DD
    api_url = f"https://api.github.com/repos/{REPO_OWNER}/{REPO_NAME}/contents/data"
    
    try:
        response = requests.get(api_url)
        response.raise_for_status()
        contents = response.json()
        
        # Filter for directories that look like dates
        dates = [item['name'] for item in contents if item['type'] == 'dir']
        # Sort descending (newest first)
        dates.sort(reverse=True)
        
        return jsonify(dates)
    except Exception as e:
        print(f"Error fetching dates: {e}")
        return jsonify([])

if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5000)
