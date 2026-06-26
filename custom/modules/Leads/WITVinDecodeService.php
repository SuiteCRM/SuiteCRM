<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class WITVinDecodeService
{
    public function enrichVehicles(array $vehicles)
    {
        foreach ($vehicles as $index => $vehicle) {
            if (empty($vehicle['vin'])) {
                continue;
            }
            $decoded = $this->decode($vehicle['vin']);
            if (!empty($decoded)) {
                $vehicles[$index] = array_merge($vehicle, $decoded);
            }
        }
        return $vehicles;
    }

    public function decode($vin, $modelYear = '')
    {
        $vin = strtoupper(trim((string)$vin));
        if (!preg_match('/^[A-HJ-NPR-Z0-9]{17}$/', $vin)) {
            return array('vin_decode_error' => 'Invalid VIN format');
        }

        $url = 'https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinValues/' . rawurlencode($vin) . '?format=json';
        if ($modelYear !== '') {
            $url .= '&modelyear=' . rawurlencode($modelYear);
        }

        $json = $this->httpGet($url);
        if (!$json) {
            return array('vin_decode_error' => 'NHTSA lookup unavailable');
        }

        $payload = json_decode($json, true);
        if (empty($payload['Results'][0]) || !is_array($payload['Results'][0])) {
            return array('vin_decode_error' => 'NHTSA returned no result');
        }

        $result = $payload['Results'][0];
        $decoded = array(
            'vin' => $vin,
            'year' => isset($result['ModelYear']) ? $result['ModelYear'] : '',
            'make' => isset($result['Make']) ? $result['Make'] : '',
            'model' => isset($result['Model']) ? $result['Model'] : '',
            'body_class' => isset($result['BodyClass']) ? $result['BodyClass'] : '',
            'vehicle_type' => isset($result['VehicleType']) ? $result['VehicleType'] : '',
            'manufacturer' => isset($result['Manufacturer']) ? $result['Manufacturer'] : '',
            'vin_decode_error_code' => isset($result['ErrorCode']) ? trim((string)$result['ErrorCode']) : '',
            'vin_decode_error' => isset($result['ErrorText']) ? trim((string)$result['ErrorText']) : '',
        );

        return array_filter($decoded, function ($value) {
            return $value !== null && $value !== '';
        });
    }

    private function httpGet($url)
    {
        if (function_exists('curl_init')) {
            $curl = curl_init($url);
            curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
            curl_setopt($curl, CURLOPT_CONNECTTIMEOUT, 4);
            curl_setopt($curl, CURLOPT_TIMEOUT, 8);
            curl_setopt($curl, CURLOPT_SSL_VERIFYPEER, true);
            $response = curl_exec($curl);
            curl_close($curl);
            return $response ?: '';
        }

        $context = stream_context_create(array(
            'http' => array('timeout' => 8),
            'ssl' => array('verify_peer' => true, 'verify_peer_name' => true),
        ));
        $response = @file_get_contents($url, false, $context);
        return $response ?: '';
    }
}
