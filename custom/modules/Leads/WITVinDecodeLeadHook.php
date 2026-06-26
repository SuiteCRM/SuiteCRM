<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'custom/modules/Leads/WITVinDecodeService.php';

class WITVinDecodeLeadHook
{
    public function beforeSave(&$bean, $event, $arguments)
    {
        if (empty($bean->vehicles_json_c)) {
            return;
        }
        $vehicles = json_decode((string)$bean->vehicles_json_c, true);
        if (!is_array($vehicles) || empty($vehicles)) {
            $bean->vin_decode_status_c = 'Vehicle data is not valid JSON';
            return;
        }
        $service = new WITVinDecodeService();
        $vehicles = $service->enrichVehicles($vehicles);
        $bean->vehicles_json_c = json_encode($vehicles, JSON_PRETTY_PRINT);
        $bean->vin_decode_status_c = $this->status($vehicles);
    }

    private function status(array $vehicles)
    {
        $parts = array();
        foreach ($vehicles as $vehicle) {
            $vin = !empty($vehicle['vin']) ? $vehicle['vin'] : 'VIN';
            if (!empty($vehicle['make']) || !empty($vehicle['model'])) {
                $parts[] = $vin . ': decoded';
            } elseif (!empty($vehicle['vin_decode_error'])) {
                $parts[] = $vin . ': ' . $vehicle['vin_decode_error'];
            } else {
                $parts[] = $vin . ': stored';
            }
        }
        return implode('; ', $parts);
    }
}
