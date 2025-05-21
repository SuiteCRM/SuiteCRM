<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$db = DBManagerFactory::getInstance();

if ($db instanceof DBManager) 
{
    // Bounce Inbound Email
    $query = "SELECT stored_options FROM inbound_email WHERE deleted = 0 AND status = 'Active' AND mailbox_type = 'bounce';";
    $res = $db->query($query);
    $row = $db->fetchByAssoc($res);
    $options = unserialize(base64_decode($row['stored_options']));
    $replyToName = $options['reply_to_name'] ?? '';
    $replyToAddr = $options['reply_to_addr'] ?? '';

    if (!empty($replyToName) || !empty($replyToAddr))
    {
        // System Outbound Email
        $query = "SELECT id FROM outbound_email WHERE deleted = 0 AND type = 'system';";
        $res = $db->query($query);
        $id = $db->fetchByAssoc($res)['id'];

        // Update the System Outbound Email
        $query = "UPDATE outbound_email SET reply_to_name = '$replyToName', reply_to_addr = '$replyToAddr' WHERE id = '$id';";
        $res = $db->query($query, true, "Error al actualizar la cuenta SMTP");
    }
} else {
    $GLOBALS['log']->fatal('Line '.__LINE__.': '.__METHOD__.': DBManager is not set');
}