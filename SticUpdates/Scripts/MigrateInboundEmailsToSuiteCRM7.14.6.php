<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');


// Update the new type field for inbound email accounts based on the value of the is_personal and mailbox_type fields. 
// This is because with the update to SuiteCMR 7.14.6 a new field ($type) appears and it is necessary to calculate it when making the migration.
$db = DBManagerFactory::getInstance();
if ($db instanceof DBManager) 
{
    // Bounce Inbound Email
    $query = "SELECT id, mailbox_type, is_personal FROM inbound_email";
    $res = $db->query($query) ?? '';
    while ($row = $db->fetchByAssoc($res)) {
        $id = $row['id'] ?? '';
        $mailbox_type = $row['mailbox_type'] ?? '';
        $is_personal = $row['is_personal'] ?? '';

        if (!empty($id)) {
            // Fill new type field
            if ($is_personal) {
                $type = 'personal';
            } else {
                $type = ($mailbox_type == 'bounce') ? 'bounce' : 'gruop';
            }

            $query = "UPDATE inbound_email SET type = '{$type}' WHERE id = '{$id}'";
            $result = $db->query($query) ?? '';
            if (!$result) {
                $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.": Failed to update record with ID = {$id}");
            }
        } else { 
            $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.': The inbound email ID to be updated has not been received');
        }
    
    }
} else {
    $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.': DBManager is not set');
}