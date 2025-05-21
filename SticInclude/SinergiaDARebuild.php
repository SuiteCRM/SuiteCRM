<?php

/**
 * Class SinergiaDARebuild
 *
 * This class is used to rebuild the Sinergia Data Analytics system.
 */
#[\AllowDynamicProperties]
class SinergiaDARebuild
{

    /**
     * Function rebuild
     *
     * Performs a rebuild operation if user is an administrator and SinnergiaDA is enabled.
     * Returns 'ok' on success, error messages on failure, or a failure message for non-admin users.
     *
     * @return string The result of the rebuild operation.
     */
    public static function rebuild($callUpdateModel = false, $rebuildFilter = 'all')
    {
        global $current_user;

        if ($callUpdateModel) {
            $_REQUEST['update_model'] = 1;
        }

        // If it's a call that has been received via the API, the user is not validated through
        // username and password, so we assign the system user to execute the 'rebuild' method of SinergiaDA.
        if (!is_admin($current_user)) {
            $current_user->getSystemUser();
        }

        require_once 'modules/stic_Settings/Utils.php';
        
        // Check if SinergiaDA is enabledfem
        global $sugar_config;
        $sdaEnabled = $sugar_config['stic_sinergiada']['enabled'] ?? false;
        if (empty($sdaEnabled) || !$sdaEnabled) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "SinergiaDA setting is not = 1.");
            die("Sinergia Data Analytics is disabled.");
            return;
        }

        // Only proceed if the current user is an admin
        if (is_admin($current_user)) {
            require_once 'SticInclude/SinergiaDA.php';

            // Set the force request parameter
            $_REQUEST['force'] = ($_REQUEST['force'] ?? null) == 1 ? true : false;

            // Create views via the ExternalReporting class
            $r = new ExternalReporting();
            $res = $r->createViews($callUpdateModel, $rebuildFilter);

            // Pattern to match fatal errors
            $pattern = "/(\[FATAL:\s*.*?\])/i";

            preg_match_all($pattern, $res, $matches);
            $msg = '';

            // Build error message from fatal error matches
            foreach ($matches[1] as $match) {
                $msg .= $match . "<br>";
            }

            if(file_exists('sdaRebuildError.txt'))
            {
                unlink('sdaRebuildError.txt');
            }
            
            // Return 'ok' or the error message
            if (empty($msg)) {
                return 'ok';
            } else {
                sugar_file_put_contents('sdaRebuildError.txt', $msg);
                return $msg;
            }
            die();
        } else {
            // Non-admin users are not allowed to perform the operation
            die('<h1>Operación restringida a administradores</h1>');
        }

        return false;
    }

    /**
     * Calls the API to rebuild SinergiaDA.
     *
     * @param bool $callUpdateModel Indicates whether to update the model.
     * @return void
     */
    public static function callApiRebuildSDA($callUpdateModel = false, $rebuildFilter = 'all')
    {
        global $sugar_config;
        $GLOBALS['log']->stic('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Rebuilding SinergiaDA via API");

        $db_user_name = $sugar_config['dbconfig']['db_user_name'];

        // Obtain the valid token
        $validToken = md5($db_user_name . date("Ymd"));

        // API endpoint URL
        $url = "{$sugar_config['site_url']}/custom/service/v4_1_SticCustom/rest.php";

        // Data to send in the request
        $data = array(
            'debug' => '1',
            'update_model' => $callUpdateModel ? 1 : 0,
            'rebuild_filter' => $rebuildFilter,
            'method' => 'rebuild_sda',
            'input_type' => 'JSON',
            'response_type' => 'JSON',
            'token' => $validToken,
        );

        // Create a curl instance
        $ch = curl_init();

        // Set the URL and other necessary options
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_POST, 1);
        curl_setopt($ch, CURLOPT_POSTFIELDS, http_build_query($data));
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_FRESH_CONNECT, true); // Perform the request in the background
        curl_setopt($ch, CURLOPT_NOSIGNAL, 1); // Ignore signals, allow the request to continue even if the user cancels

        // Execute the request and get the response
        $res = curl_exec($ch);

        // Check if any error occurred
        if (curl_errno($ch)) {
            $GLOBALS['log']->stic('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Rebuilding SinergiaDA via API. ERROR " . curl_error($ch));
        } else{
            $GLOBALS['log']->stic('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Rebuilding SinergiaDA via API. OK " );

        }

        // Close the curl instance
        curl_close($ch);

        $GLOBALS['log']->stic('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Rebuilding SinergiaDA via API. END");
    }

}
