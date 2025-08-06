#!/bin/bash
# Start both the REST API server and WebSocket server

cd /app

# Start REST API server
echo "Starting REST API server on port 3000..."
node server.js &
API_PID=$!

# Start WebSocket server
echo "Starting WebSocket server on port 8081..."
node websocket-server.js &
WS_PID=$!

# Wait for both processes
echo "Servers started: API=$API_PID, WebSocket=$WS_PID"
wait $API_PID $WS_PID