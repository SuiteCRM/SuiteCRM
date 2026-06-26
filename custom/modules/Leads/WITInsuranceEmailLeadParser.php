<?php
if (!defined('sugarEntry') || !sugarEntry) { die('Not A Valid Entry Point'); }
require_once 'custom/modules/Leads/WITVinDecodeService.php';

class WITInsuranceEmailLeadParser
{
    public function run()
    {
        $db = DBManagerFactory::getInstance();
        $sql = "SELECT e.id, e.name, e.description, e.description_html, e.from_addr, e.to_addrs
                FROM emails e LEFT JOIN emails_cstm ec ON ec.id_c = e.id
                WHERE e.deleted = 0
                AND (ec.wit_parsed_to_lead_c IS NULL OR ec.wit_parsed_to_lead_c = 0)
                AND (LOWER(COALESCE(e.from_addr,'')) LIKE '%leads@weinsurethings.com%'
                  OR LOWER(COALESCE(e.to_addrs,'')) LIKE '%leads@weinsurethings.com%'
                  OR LOWER(COALESCE(e.from_addr,'')) LIKE '%leads@weinsruethings.com%'
                  OR LOWER(COALESCE(e.to_addrs,'')) LIKE '%leads@weinsruethings.com%'
                  OR LOWER(COALESCE(e.name,'')) LIKE '%quote request%'
                  OR LOWER(COALESCE(e.name,'')) LIKE '%insurance lead%')
                ORDER BY e.date_entered ASC LIMIT 25";
        $result = $db->query($sql);
        $count = 0;
        while ($email = $db->fetchByAssoc($result)) {
            $parsed = $this->parseEmail($email);
            if (!$parsed['email'] && !$parsed['phone'] && !$parsed['name'] && empty($parsed['vehicles'])) {
                $this->markEmail($email['id'], '', 'Skipped: not enough lead data');
                continue;
            }
            $lead = $this->saveLead($parsed, $email);
            $this->markEmail($email['id'], $lead->id, 'Parsed into lead');
            $count++;
        }
        $GLOBALS['log']->info('WITInsuranceEmailLeadParser processed ' . $count . ' email(s).');
        return true;
    }

    private function parseEmail($email)
    {
        $text = $this->clean(($email['name'] ?: '') . "\n" . ($email['from_addr'] ?: '') . "\n" . ($email['description'] ?: '') . "\n" . ($email['description_html'] ?: ''));
        $vehicles = $this->extractVehicles($text);
        if ($vehicles) {
            $vinService = new WITVinDecodeService();
            $vehicles = $vinService->enrichVehicles($vehicles);
        }
        $name = $this->label($text, array('name', 'insured name', 'customer name', 'applicant name')) ?: $this->nameFromAddress($email['from_addr']);
        $convictionDate = $this->dateLabel($text, array('conviction date', 'convicted', 'violation date'));
        return array(
            'name' => $name,
            'email' => $this->email($text . ' ' . $email['from_addr']),
            'phone' => $this->phone($text),
            'address' => $this->label($text, array('address', 'risk address', 'garaging address')),
            'coverage_limits' => $this->label($text, array('coverage limits', 'limits', 'liability limits')),
            'call_summary' => $this->label($text, array('call summary', 'summary', 'notes')) ?: substr($text, 0, 2500),
            'next_action' => $this->label($text, array('next action', 'action item', 'to do')),
            'next_follow_up_date' => $this->dateLabel($text, array('follow up', 'next follow up', 'call back')),
            'conviction_date' => $convictionDate,
            'policy_type' => $this->policyType($text),
            'drivers' => $this->drivers($text, $name),
            'vehicles' => $vehicles,
            'incidents' => $this->incidents($text, $convictionDate),
            'confidence' => $this->confidence($name, $vehicles),
            'raw_text' => $text,
        );
    }

    private function saveLead($p, $email)
    {
        $lead = BeanFactory::newBean('Leads');
        $lead->lead_source = 'Email';
        $lead->status = 'New';
        $parts = $this->splitName($p['name']);
        $lead->first_name = $parts['first'];
        $lead->last_name = $parts['last'] ?: 'Unknown';
        $lead->email1 = $p['email'];
        $lead->phone_mobile = $p['phone'];
        $lead->primary_address_street = $p['address'];
        $lead->policy_type_c = $p['policy_type'];
        $lead->coverage_limits_c = $p['coverage_limits'];
        $lead->drivers_json_c = json_encode($p['drivers'], JSON_PRETTY_PRINT);
        $lead->vehicles_json_c = json_encode($p['vehicles'], JSON_PRETTY_PRINT);
        $lead->accidents_violations_json_c = json_encode($p['incidents'], JSON_PRETTY_PRINT);
        $lead->call_summary_c = $p['call_summary'];
        $lead->next_action_c = $p['next_action'];
        if ($p['next_follow_up_date']) { $lead->next_follow_up_date_c = $p['next_follow_up_date'] . ' 09:00:00'; }
        if ($p['conviction_date']) { $lead->last_violation_conviction_date_c = $p['conviction_date']; }
        $lead->lead_source_email_id_c = $email['id'];
        $lead->email_parse_confidence_c = $p['confidence'];
        $lead->vin_decode_status_c = $this->vinStatus($p['vehicles']);
        $lead->description = "Imported from leads mailbox.\n\nSubject: " . $email['name'] . "\n\n" . $p['raw_text'];
        $lead->save();
        return $lead;
    }

    private function markEmail($emailId, $leadId, $status)
    {
        $email = BeanFactory::getBean('Emails', $emailId);
        if (!$email || empty($email->id)) { return; }
        $email->wit_parsed_to_lead_c = 1;
        $email->wit_created_lead_id_c = $leadId;
        $email->wit_email_parse_status_c = $status;
        $email->save();
    }

    private function clean($text) { $text = html_entity_decode((string)$text, ENT_QUOTES, 'UTF-8'); $text = preg_replace('/<br\s*\/?>/i', "\n", $text); $text = strip_tags($text); return trim(preg_replace('/[ \t]+/', ' ', $text)); }
    private function label($text, $labels) { foreach ($labels as $label) { if (preg_match('/(?:^|\n)\s*' . preg_quote($label, '/') . '\s*[:\-]\s*(.+?)(?=\n\s*[A-Za-z][A-Za-z\s]{2,30}\s*[:\-]|\n{2,}|$)/is', $text, $m)) { return trim($m[1]); } } return ''; }
    private function dateLabel($text, $labels) { foreach ($labels as $label) { if (preg_match('/' . preg_quote($label, '/') . '(?:\s*on|\s*date)?\s*[:\-]?\s*(\d{1,2}[\/\-]\d{1,2}[\/\-]\d{2,4})/i', $text, $m)) { try { return (new DateTime($m[1]))->format('Y-m-d'); } catch (Exception $e) { return ''; } } } return ''; }
    private function email($text) { if (!preg_match_all('/[A-Z0-9._%+\-]+@[A-Z0-9.\-]+\.[A-Z]{2,}/i', $text, $m)) { return ''; } foreach ($m[0] as $email) { $email = strtolower($email); if (strpos($email, 'weinsurethings.com') === false && strpos($email, 'weinsruethings.com') === false) { return $email; } } return strtolower($m[0][0]); }
    private function phone($text) { if (preg_match('/\b(?:\+?1[\s\.\-]?)?\(?\d{3}\)?[\s\.\-]\d{3}[\s\.\-]\d{4}\b/', $text, $m)) { return trim($m[0]); } return ''; }
    private function extractVehicles($text) { $vehicles = array(); if (preg_match_all('/\b[A-HJ-NPR-Z0-9]{17}\b/i', $text, $m)) { foreach (array_unique($m[0]) as $vin) { $vehicles[] = array('vin' => strtoupper($vin)); } } return $vehicles; }
    private function drivers($text, $name) { $d = array(); if ($name) { $d['name'] = $name; } $dob = $this->label($text, array('dob', 'date of birth')); $dl = $this->label($text, array('dl', 'driver license', 'drivers license')); if ($dob) { $d['dob'] = $dob; } if ($dl) { $d['driver_license'] = $dl; } return $d ? array($d) : array(); }
    private function incidents($text, $convictionDate) { $l = strtolower($text); if (strpos($l, 'accident') === false && strpos($l, 'violation') === false && strpos($l, 'ticket') === false && strpos($l, 'conviction') === false) { return array(); } $i = array('type' => strpos($l, 'accident') !== false ? 'accident' : 'violation', 'description' => substr($text, 0, 750)); if ($convictionDate) { $i['conviction_date'] = $convictionDate; } return array($i); }
    private function policyType($text) { $t = strtolower($text); if (strpos($t, 'commercial auto') !== false || strpos($t, 'box truck') !== false || strpos($t, 'tow truck') !== false) return 'commercial_auto'; if (strpos($t, 'workers comp') !== false) return 'workers_comp'; if (strpos($t, 'general liability') !== false) return 'general_liability'; if (strpos($t, 'home') !== false) return 'home'; if (strpos($t, 'renters') !== false) return 'renters'; if (strpos($t, 'auto') !== false || strpos($t, 'vin') !== false || preg_match('/\b[A-HJ-NPR-Z0-9]{17}\b/i', $text)) return 'personal_auto'; return 'unknown'; }
    private function splitName($name) { $parts = preg_split('/\s+/', trim((string)$name)); if (!$parts || $parts[0] === '') return array('first' => '', 'last' => 'Unknown'); if (count($parts) === 1) return array('first' => '', 'last' => $parts[0]); $last = array_pop($parts); return array('first' => implode(' ', $parts), 'last' => $last); }
    private function nameFromAddress($from) { if (preg_match('/^([^<@\n]+)\s*</', (string)$from, $m)) { return trim(str_replace(array('"', "'"), '', $m[1])); } return ''; }
    private function confidence($name, $vehicles) { return ($name && $vehicles) ? 'high' : ($name || $vehicles ? 'medium' : 'low'); }
    private function vinStatus($vehicles) { $out = array(); foreach ($vehicles as $v) { $out[] = (isset($v['vin']) ? $v['vin'] : 'VIN') . (!empty($v['make']) || !empty($v['model']) ? ': decoded' : ': stored'); } return implode('; ', $out); }
}
