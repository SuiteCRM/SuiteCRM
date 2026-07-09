<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
// Prevents directly accessing this file from a web browser
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Service class responsible for evaluating AWF submissions to detect SPAM.
 * Implements a multi-layered defense: Dynamic Honeypot, Secure TimeTrap (HMAC), and Header Heuristics.
 * Maintains backward compatibility (Legacy Mode) for V1 forms.
 */
class AntiSpamService
{
    /**
     * Main entry point to evaluate if a request is SPAM.
     * @param array $postData The $_POST data from the form submission
     * @param array $serverData The $_SERVER data from the request
     * @return AntiSpamResult The result of the SPAM evaluation, including flags and messages
     */
    public function checkRequest(array $postData, array $serverData): AntiSpamResult {
        $result = new AntiSpamResult();

        // 1. Evaluate Request Integrity (Heuristics)
        if ($this->checkRequestIntegrity($serverData)) {
            $result->isSpam = true;
            $result->userDescription = translate('LBL_RESPONSE_USERAGENT_SPAM', 'stic_AWF_Responses');
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Spam detected by Request Integrity (Headers) check");
            return $result;
        }

        // 2. Evaluate Honeypot (V1 & V2)
        if ($this->checkHoneypot($postData)) {
            $result->isSpam = true;
            $result->userDescription = translate('LBL_RESPONSE_HONEYPOT_SPAM', 'stic_AWF_Responses');
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Spam detected by Honeypot protection");
            return $result;
        }

        // 3. Evaluate TimeTrap (V1 & V2) with microtime
        $timeTrapResult = $this->checkTimeTrap($postData);
        if ($timeTrapResult['isSpam']) {
            $result->isSpam = true;
            $result->userDescription = translate('LBL_RESPONSE_TIMETRAP_SPAM', 'stic_AWF_Responses') . " ({$timeTrapResult['duration']}s)";
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Spam detected by TimeTrap protection. Duration: {$timeTrapResult['duration']}s");
            return $result;
        }

        return $result;
    }

    /**
     * Checks for the presence of honeypot fields and whether they are filled.
     * Supports both V1 (fixed field) and V2 (dynamic fields with prefix) honeypot implementations.
     * @param array $postData The $_POST data from the form submission
     * @return bool True if the honeypot check indicates SPAM, false otherwise
     */
    private function checkHoneypot(array $postData): bool {
        // V1 Legacy check
        if (array_key_exists('awf_honey_pot', $postData)) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Fixed Honeypot field found: 'awf_honey_pot'");

            if (!empty($postData['awf_honey_pot'])) {
                $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Fixed Honeypot field is not empty: 'awf_honey_pot'");
                return true;
            }
        }

        // V2 Dynamic check (Iterate looking for the semantic bait prefix)
        foreach ($postData as $key => $value) {
            if (strpos($key, 'awf_website_url_') === 0) {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Dynamic Honeypot field found: '{$key}'");

                if ($value !== '') {
                    $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Dynamic Honeypot field is not empty: '{$key}'");
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Checks the TimeTrap fields to determine if the submission was too fast (indicative of bots) or too old (possible replay attack).
     * Supports both V1 (legacy timestamp) and V2 (HMAC-secured timestamp) implementations, using microtime for better precision.
     * @param array $postData The $_POST data from the form submission
     * @return array An associative array with 'isSpam' (bool) and 'duration' (float) indicating the result of the TimeTrap check and the duration in seconds
     */
    private function checkTimeTrap(array $postData): array {
        // We capture as string to prevent float precision drift during HMAC hashing
        $submissionTsStr = isset($postData['awf_submission_ts']) ? (string)$postData['awf_submission_ts'] : '0';
        $submissionToken = isset($postData['awf_submission_token']) ? $postData['awf_submission_token'] : null;
        
        $submissionFloat = (float)$submissionTsStr;
        $currentTs = microtime(true);
        $duration = round($currentTs - $submissionFloat, 2);

        $result = ['isSpam' => false, 'duration' => $duration];

        // Ensure fallback for $duration if submission is literally 0
        if ($submissionFloat === 0.0) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Submission timestamp is zero, treating as potential manipulation.");

            $result['isSpam'] = true;
            return $result;
        }

        // V1: Legacy mode without token
        if ($submissionToken === null) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Legacy TimeTrap check. Duration: {$duration}s");

            // Allow negative duration (unsynced client clocks) but catch 0 to 1.99s
            if ($duration >= 0.0 && $duration < 2.0) {
                $result['isSpam'] = true;
                return $result;
            }

            return $result;
        }

        // V2: Secure Mode with HMAC
        global $sugar_config;
        $secretKey = $sugar_config['unique_key'] ?? 'default_fallback_key';
        
        // Re-calculate the expected signature using the exact string provided
        $expectedToken = hash_hmac('sha256', $submissionTsStr, $secretKey);

        // Use hash_equals to prevent timing attacks
        if (!hash_equals($expectedToken, $submissionToken)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: HMAC token manipulation detected.");
            $result['isSpam'] = true;
            return $result;
        }

        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Secure TimeTrap check. Duration: {$duration}s");

        // Block if faster than 2 seconds or older than 48 hours (172800s TTL)
        if ($duration < 2.0 || $duration > 172800.0) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: TimeTrap violation detected. Duration: {$duration}s");
            $result['isSpam'] = true;
            return $result;
        }

        return $result;
    }

    /**
     * Evaluates the request headers for common SPAM indicators, such as suspicious User-Agent strings or missing headers.
     * This is a heuristic check and can produce false positives, but helps catch basic bots that don't mimic real browsers.
     * @param array $serverData The $_SERVER data from the request
     * @return bool True if the header analysis indicates SPAM, false otherwise
     */
    private function checkRequestIntegrity(array $serverData): bool {
        $userAgent = $serverData['HTTP_USER_AGENT'] ?? '';

        if ($this->isBotUserAgent($userAgent)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Suspicious User-Agent detected: '{$userAgent}'");
            return true;
        }

        $acceptLanguage = $serverData['HTTP_ACCEPT_LANGUAGE'] ?? '';
        if (empty($acceptLanguage)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Missing Accept-Language header. User-Agent: '{$userAgent}'");
            return true;
        }

        $acceptEncoding = $serverData['HTTP_ACCEPT_ENCODING'] ?? '';
        if (empty($acceptEncoding)) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ": AntiSpamService: Missing Accept-Encoding header. User-Agent: '{$userAgent}'");
            return true;
        }

        return false;
    }

    /**
     * Determines if the User Agent string belongs to a bot or script. This is used for spam detection and to apply different duplicate detection rules.
     * The function checks if the User Agent is empty (which is suspicious) 
     *  or if it contains known signatures of programming tools and libraries commonly used for making HTTP requests (like curl, wget, python, java, etc.).
     * If any of these conditions are met, it returns true, indicating that the User Agent is likely a bot. 
     * Otherwise, it returns false, indicating that it is likely a human user.
     * @param string $userAgent The User Agent string from the request headers
     * @return bool True if the User Agent is identified as a bot, false otherwise
     */
    private function isBotUserAgent(string $userAgent): bool {
        // If it's empty, it's suspicious (all browsers send something)
        if (empty($userAgent)) {
            return true; 
        }

        // Blacklist of programming tools that are NOT browsers
        // If the User Agent contains any of these words, it is a script.
        $botSignatures = [
            'curl',          // Linux command tool
            'wget',          // Download tool
            'python',        // Requests/Ulllib library
            'java/',         // Java HTTP Client
            'libwww',        // Perl library
            'httpclient',    // Generic Apache/Java
            'php/',          // PHP scripts (file_get_contents)
            'postman',       // API testing tool
            'insomnia',      // API testing tool
            'node-fetch',    // NodeJS
            'axios',         // JS library (server side)
            'go-http-client' // Golang
        ];

        foreach ($botSignatures as $bot) {
            if (strpos($userAgent, $bot) !== false) {
                return true;
            }
        }

        return false;
    }
}