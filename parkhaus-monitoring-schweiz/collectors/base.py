"""
Base parking collector class for all city-specific collectors.
"""

import requests
import json
import os
from datetime import datetime
from abc import ABC, abstractmethod


class BaseParkingCollector(ABC):
    """Abstract base class for parking data collectors."""
    
    def __init__(self, city_id, city_name, api_url, data_dir="data"):
        """
        Initialize the collector.
        
        Args:
            city_id: City identifier (e.g., 'luzern', 'basel')
            city_name: Display name of the city
            api_url: API endpoint URL
            data_dir: Base directory for data storage
        """
        self.city_id = city_id
        self.city_name = city_name
        self.api_url = api_url
        self.data_dir = data_dir
        self.city_data_dir = os.path.join(data_dir, city_id)
    
    def fetch_raw_data(self):
        """
        Fetch raw data from the API.
        
        Returns:
            dict: Raw API response as JSON
        
        Raises:
            requests.RequestException: If the API request fails
        """
        try:
            response = requests.get(self.api_url, timeout=10)
            response.raise_for_status()
            return response.json()
        except requests.RequestException as e:
            print(f"[{datetime.now()}] Error fetching data for {self.city_name}: {e}")
            raise
    
    @abstractmethod
    def normalize_data(self, raw_data):
        """
        Convert raw API data to unified format.
        
        Args:
            raw_data: Raw data from API
        
        Returns:
            dict: Normalized data in unified format:
            {
                "status": "success",
                "city": "city_id",
                "data": {
                    "parkings": {
                        "PARKING_ID": {
                            "id": "PARKING_ID",
                            "name": "Parking Name",
                            "free": 150,
                            "total": 200,
                            "status": "open",
                            "timestamp": "2026-01-06T07:30:00+01:00"
                        }
                    }
                },
                "timestamp": "2026-01-06T07:30:00+01:00"
            }
        """
        pass
    
    def save_data(self, data):
        """
        Save normalized data to JSON file.
        
        Args:
            data: Normalized parking data
        """
        if not data:
            return
        
        now = datetime.now()
        date_str = now.strftime("%Y-%m-%d")
        time_str = now.strftime("%H-%M-%S")
        
        # Create directory for today if it doesn't exist
        day_dir = os.path.join(self.city_data_dir, date_str)
        os.makedirs(day_dir, exist_ok=True)
        
        filename = f"{time_str}.json"
        filepath = os.path.join(day_dir, filename)
        
        try:
            with open(filepath, "w", encoding="utf-8") as f:
                json.dump(data, f, indent=2, ensure_ascii=False)
            print(f"[{now}] {self.city_name}: Data saved to {filepath}")
        except IOError as e:
            print(f"[{now}] {self.city_name}: Error saving data: {e}")
    
    def collect(self):
        """
        Main collection method: fetch, normalize, and save data.
        
        Returns:
            bool: True if successful, False otherwise
        """
        try:
            print(f"[{datetime.now()}] {self.city_name}: Fetching data...")
            raw_data = self.fetch_raw_data()
            
            print(f"[{datetime.now()}] {self.city_name}: Normalizing data...")
            normalized_data = self.normalize_data(raw_data)
            
            print(f"[{datetime.now()}] {self.city_name}: Saving data...")
            self.save_data(normalized_data)
            
            return True
        except Exception as e:
            print(f"[{datetime.now()}] {self.city_name}: Collection failed: {e}")
            return False
