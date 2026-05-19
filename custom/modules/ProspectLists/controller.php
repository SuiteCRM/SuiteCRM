<?php

class CustomProspectListsController extends SugarController
{

    /**
     * Action to create an automatic List of Prospects (LPO) based on a notification campaign record.
     * It accepts parameters such as recordID, type of LPO, label, and module name from the REQUEST.
     * The action attempts to include custom LPO types first, falling back to standard types if necessary.
     *
     * @return void
     */
    public function action_createAutoLpo()
    {
        $id = $_REQUEST['id'] ?? '';
        $type = $_REQUEST['filterName'] ?? '';
        $label = $_REQUEST['label'] ?? 'LPO';
        $module = $_REQUEST['filterModule'];

        if (empty($id) || empty($type) || empty($module) || empty($label)) {
            die(json_encode(['status' => 'error', 'message' => 'Missing required parameters.']));
        }
        $result = ['status' => 'error'];
        // Attempt to include custom LPO types first
        if (file_exists("custom/modules/{$module}/CustomLPOTypes.php")) {
            require_once "custom/modules/{$module}/CustomLPOTypes.php";
            $result = CustomLPOTypes::generateLPO($id, $type, $label);
        }

        // For subpanel notifications, get all aplicants of the related offer
        if ($result['status'] != 'success' && $module === 'stic_Job_Offers') {
            require_once 'modules/stic_Job_Offers/Utils.php';
            $result = stic_Job_OffersUtils::generateApplicantsLpo($id, $label, $type);
        }

        // Fallback to standard LPO types if custom fails or does not exist
        if ($result['status'] != 'success' && file_exists("modules/{$module}/LPOTypes.php")) {
            require_once "modules/{$module}/LPOTypes.php";
            $result = LPOTypes::generateLPO($id, $type, $label);
        }

        die(json_encode($result));
    }

    
    
    /**
     * Action to populate LPO filter options.
     * It retrieves predefined filter types and their labels, encodes them in JSON format, and outputs the result.
     *
     * @return void
     */
    public function action_populateLPOFilters()
    {
        global $mod_strings;
        $types = [
            'stic_Signatures__all_signers' => $mod_strings['LBL_LPO_STIC_SIGNATURES_ALL_SIGNERS'],
            'stic_Signatures__signers_pending' => $mod_strings['LBL_LPO_STIC_SIGNATURES_PENDING_SIGNERS'],
            'stic_Events__registrations_confirmed' => $mod_strings['LBL_LPO_STIC_EVENTS_CONFIRMED_REGISTRATIONS'],
            'stic_Job_Offers__all_applicants' => $mod_strings['LBL_LPO_STIC_JOB_OFFERS_ALL_APPLICANTS'],
            'stic_Job_Offers__active_applicants' => $mod_strings['LBL_LPO_STIC_JOB_OFFERS_ACTIVE_APPLICANTS'],
        ];
        echo json_encode($types);
        die();
    }

}
