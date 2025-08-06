<?php
/**
 * COBOL WebSocket Configuration
 * Central configuration for WebSocket connectivity
 */

// WebSocket port configuration
$cobol_websocket_config = array(
    // Primary WebSocket port for real-time COBOL connections
    'websocket_port' => 8081,
    
    // WebSocket URL endpoints
    'websocket_url' => 'ws://localhost:8081',
    'websocket_url_docker' => 'ws://cobol-gateway:8081',
    
    // REST API configuration
    'rest_api_port' => 8080,
    'rest_api_url' => 'http://localhost:8080',
    'rest_api_url_docker' => 'http://cobol-gateway:8080',
    
    // Connection settings
    'connection_timeout' => 30,
    'ping_interval' => 30,
    'reconnect_attempts' => 3,
    'reconnect_delay' => 5,
    
    // Security settings
    'require_authentication' => true,
    'auth_token_expiry' => 3600, // 1 hour
    
    // Message settings
    'max_message_size' => 1048576, // 1MB
    'compression_enabled' => false,
    
    // Logging
    'debug_mode' => false,
    'log_websocket_messages' => false
);

// Export to global config if SuiteCRM config is available
if (isset($sugar_config)) {
    $sugar_config['cobol_websocket_port'] = $cobol_websocket_config['websocket_port'];
    $sugar_config['cobol_websocket_url'] = $cobol_websocket_config['websocket_url'];
    $sugar_config['cobol_rest_port'] = $cobol_websocket_config['rest_api_port'];
    $sugar_config['cobol_rest_url'] = $cobol_websocket_config['rest_api_url'];
    
    // Use Docker URLs if running in container
    if (getenv('DOCKER_CONTAINER') === 'true' || file_exists('/.dockerenv')) {
        $sugar_config['cobol_websocket_url'] = $cobol_websocket_config['websocket_url_docker'];
        $sugar_config['cobol_rest_url'] = $cobol_websocket_config['rest_api_url_docker'];
    }
}

// Helper function to get WebSocket URL
function getCobolWebSocketUrl() {
    global $cobol_websocket_config;
    
    // Check if running in Docker
    if (getenv('DOCKER_CONTAINER') === 'true' || file_exists('/.dockerenv')) {
        return $cobol_websocket_config['websocket_url_docker'];
    }
    
    return $cobol_websocket_config['websocket_url'];
}

// Helper function to get REST API URL
function getCobolRestApiUrl() {
    global $cobol_websocket_config;
    
    // Check if running in Docker
    if (getenv('DOCKER_CONTAINER') === 'true' || file_exists('/.dockerenv')) {
        return $cobol_websocket_config['rest_api_url_docker'];
    }
    
    return $cobol_websocket_config['rest_api_url'];
}

// Export configuration
return $cobol_websocket_config;