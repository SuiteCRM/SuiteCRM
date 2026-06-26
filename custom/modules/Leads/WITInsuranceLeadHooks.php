<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class WITInsuranceLeadHooks
{
    private static $afterSaveProcessed = array();

    public function beforeSave(&$bean, $event, $arguments)
    {
        $summary = $this->cleanText($this->field($bean, 'call_summary_c') . "\n" . $this->field($bean, 'description'));

        if (!$this->field($bean, 'coverage_limits_c')) {
            $bean->coverage_limits_c = $this->extractLabel($summary, array('coverage limits', 'limits', 'liability limits', 'requested limits'));
        }

        if (!$this->field($bean, 'next_action_c')) {
            $bean->next_action_c = $this->extractLabel($summary, array('next action', 'action item', 'follow up task', 'to do'));
        }

        if (!$this->field($bean, 'next_follow_up_date_c')) {
            $followUp = $this->extractDate($summary, array('follow up', 'next follow up', 'call back'));
            if ($followUp) {
                $bean->next_follow_up_date_c = $followUp . ' 09:00:00';
            }
        }

        if (!$this->field($bean, 'last_violation_conviction_date_c')) {
            $convictionDate = $this->extractConvictionFromJson($this->field($bean, 'accidents_violations_json_c'));
            if (!$convictionDate) {
                $convictionDate = $this->extractDate($summary, array('conviction date', 'convicted', 'violation date', 'violation conviction date'));
            }
            if ($convictionDate) {
                $bean->last_violation_conviction_date_c = $convictionDate;
            }
        }

        if ($this->field($bean, 'last_violation_conviction_date_c')) {
            $bean->violation_followup_date_c = $this->plusTwoYearsElevenMonths($bean->last_violation_conviction_date_c);
            $bean->auto_followup_required_c = 1;
        }

        if (!$this->field($bean, 'drivers_json_c')) {
            $driver = array();
            $dob = $this->extractLabel($summary, array('dob', 'date of birth', 'driver dob'));
            $dl = $this->extractLabel($summary, array('dl', 'driver license', 'drivers license', 'license number'));
            if ($dob) {
                $driver['dob'] = $this->normalizeDate($dob) ?: $dob;
            }
            if ($dl) {
                $driver['driver_license'] = $dl;
            }
            if (!empty($driver)) {
                $bean->drivers_json_c = json_encode(array($driver), JSON_PRETTY_PRINT);
            }
        }

        if (!$this->field($bean, 'vehicles_json_c')) {
            $vehicles = array();
            foreach ($this->extractVins($summary) as $vin) {
                $vehicles[] = array('vin' => $vin);
            }
            if ($vehicles) {
                $bean->vehicles_json_c = json_encode($vehicles, JSON_PRETTY_PRINT);
            }
        }

        $bean->missing_info_c = implode("\n", $this->missingInfo($bean));
    }

    public function afterSave(&$bean, $event, $arguments)
    {
        if (empty($bean->id) || isset(self::$afterSaveProcessed[$bean->id])) {
            return;
        }
        self::$afterSaveProcessed[$bean->id] = true;
        $assignedUserId = !empty($bean->assigned_user_id) ? $bean->assigned_user_id : $GLOBALS['current_user']->id;

        if ($this->field($bean, 'next_follow_up_date_c')) {
            $this->createTaskIfMissing($bean, 'WIT Follow Up: ' . $this->displayName($bean), $bean->next_follow_up_date_c, $this->field($bean, 'next_action_c') ?: 'Follow up with this insurance lead.', $assignedUserId);
        }

        if ($this->field($bean, 'violation_followup_date_c')) {
            $this->createTaskIfMissing($bean, 'WIT Violation Re-Shop Follow Up: ' . $this->displayName($bean), $bean->violation_followup_date_c . ' 09:00:00', 'Review eligibility and re-shop 2 years and 11 months after the conviction date.', $assignedUserId);
        }
    }

    private function field($bean, $field)
    {
        return isset($bean->$field) ? trim((string)$bean->$field) : '';
    }

    private function cleanText($text)
    {
        $text = html_entity_decode((string)$text, ENT_QUOTES, 'UTF-8');
        $text = strip_tags($text);
        $text = preg_replace('/[ \t]+/', ' ', $text);
        $text = preg_replace('/\r\n|\r/', "\n", $text);
        return trim($text);
    }

    private function extractLabel($text, array $labels)
    {
        foreach ($labels as $label) {
            $pattern = '/(?:^|\n)\s*' . preg_quote($label, '/') . '\s*[:\-]\s*(.+?)(?=\n\s*[A-Za-z][A-Za-z\s]{2,30}\s*[:\-]|\n{2,}|$)/is';
            if (preg_match($pattern, $text, $m)) {
                return trim($m[1]);
            }
        }
        return '';
    }

    private function extractDate($text, array $labels)
    {
        foreach ($labels as $label) {
            $pattern = '/' . preg_quote($label, '/') . '(?:\s*on|\s*date)?\s*[:\-]?\s*(\d{1,2}[\/\-]\d{1,2}[\/\-]\d{2,4})/i';
            if (preg_match($pattern, $text, $m)) {
                return $this->normalizeDate($m[1]);
            }
        }
        return '';
    }

    private function normalizeDate($value)
    {
        try {
            $date = new DateTime(trim((string)$value));
            return $date->format('Y-m-d');
        } catch (Exception $e) {
            return '';
        }
    }

    private function extractConvictionFromJson($json)
    {
        $data = json_decode((string)$json, true);
        if (!is_array($data)) {
            return '';
        }
        foreach ($data as $item) {
            if (!is_array($item)) {
                continue;
            }
            foreach (array('conviction_date', 'convictionDate', 'date') as $key) {
                if (!empty($item[$key])) {
                    $date = $this->normalizeDate($item[$key]);
                    if ($date) {
                        return $date;
                    }
                }
            }
        }
        return '';
    }

    private function plusTwoYearsElevenMonths($convictionDate)
    {
        try {
            $date = new DateTime($convictionDate);
            $date->modify('+2 years +11 months');
            return $date->format('Y-m-d');
        } catch (Exception $e) {
            return '';
        }
    }

    private function extractVins($text)
    {
        if (!preg_match_all('/\b[A-HJ-NPR-Z0-9]{17}\b/i', $text, $m)) {
            return array();
        }
        $out = array();
        foreach ($m[0] as $vin) {
            $out[strtoupper($vin)] = strtoupper($vin);
        }
        return array_values($out);
    }

    private function missingInfo($bean)
    {
        $missing = array();
        if (!$this->field($bean, 'first_name') && !$this->field($bean, 'last_name')) {
            $missing[] = 'Name';
        }
        if (!$this->field($bean, 'email1')) {
            $missing[] = 'Email';
        }
        if (!$this->field($bean, 'phone_mobile') && !$this->field($bean, 'phone_work')) {
            $missing[] = 'Phone';
        }
        if (!$this->field($bean, 'primary_address_street')) {
            $missing[] = 'Address';
        }
        if (!$this->field($bean, 'coverage_limits_c')) {
            $missing[] = 'Coverage limits';
        }
        if (!$this->field($bean, 'drivers_json_c')) {
            $missing[] = 'Driver DOB/DL';
        }
        if (!$this->field($bean, 'vehicles_json_c')) {
            $missing[] = 'VIN / vehicle information';
        }
        if (!$this->field($bean, 'next_action_c')) {
            $missing[] = 'Next action';
        }
        if (!$this->field($bean, 'next_follow_up_date_c')) {
            $missing[] = 'Scheduled follow-up';
        }
        return $missing;
    }

    private function createTaskIfMissing($lead, $name, $dateDue, $description, $assignedUserId)
    {
        $dateOnly = substr((string)$dateDue, 0, 10);
        if (!$dateOnly || $this->taskExists($lead->id, $name, $dateOnly)) {
            return;
        }
        $task = BeanFactory::newBean('Tasks');
        $task->name = $name;
        $task->status = 'Not Started';
        $task->priority = 'High';
        $task->date_due = $dateDue;
        $task->description = $description;
        $task->parent_type = 'Leads';
        $task->parent_id = $lead->id;
        $task->assigned_user_id = $assignedUserId;
        $task->save();
    }

    private function taskExists($leadId, $name, $dateOnly)
    {
        $db = DBManagerFactory::getInstance();
        $leadId = $db->quote($leadId);
        $name = $db->quote($name);
        $dateOnly = $db->quote($dateOnly);
        $sql = "SELECT id FROM tasks WHERE deleted = 0 AND parent_type = 'Leads' AND parent_id = '{$leadId}' AND name = '{$name}' AND DATE(date_due) = '{$dateOnly}' LIMIT 1";
        $result = $db->query($sql);
        return (bool)$db->fetchByAssoc($result);
    }

    private function displayName($bean)
    {
        $name = trim($this->field($bean, 'first_name') . ' ' . $this->field($bean, 'last_name'));
        return $name ?: 'Lead';
    }
}
