#!/bin/bash

echo "🚀 Starting SuiteCRM Demo Environment"
echo "===================================="

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Docker is not running. Please start Docker Desktop first."
    echo ""
    echo "To start Docker:"
    echo "1. Open Docker Desktop application"
    echo "2. Wait for Docker to start (icon in menu bar)"
    echo "3. Run this script again"
    exit 1
fi

echo "✅ Docker is running"

# Navigate to SuiteCRM directory
cd "$(dirname "$0")"

# Stop any existing containers
echo "🛑 Stopping any existing containers..."
docker-compose down 2>/dev/null

# Start SuiteCRM
echo "🚀 Starting SuiteCRM services..."
docker-compose up -d

# Wait for services to be ready
echo "⏳ Waiting for services to start..."
sleep 10

# Check if services are running
if docker ps | grep -q suitecrm-mobile; then
    echo "✅ SuiteCRM is running!"
    echo ""
    echo "🌐 Access SuiteCRM at: http://localhost:8082"
    echo "📋 Install helper at: http://localhost:8082/install_helper.php"
    echo ""
    echo "Default credentials (if fresh install):"
    echo "Username: admin"
    echo "Password: admin"
    echo ""
    echo "📦 Module packages are in: upload/upgrades/module/"
    echo ""
    echo "Would you like to open SuiteCRM in your browser? (y/n)"
    read -r response
    if [[ "$response" == "y" ]]; then
        open "http://localhost:8082/install_helper.php"
    fi
else
    echo "❌ Error: SuiteCRM failed to start"
    echo "Check logs with: docker-compose logs"
fi