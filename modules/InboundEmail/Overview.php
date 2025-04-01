<?php
/**
 * Simple class to mirror the passed object from an imap_fetch_overview() call
 */
#[\AllowDynamicProperties]
class Overview
{
    public $subject;
    public $from;
    public $fromaddr;
    public $to;
    public $toaddr;
    public $date;
    public $message_id;
    public $size;
    public $uid;
    public $msgno;
    public $recent;
    public $flagged;
    public $answered;
    public $deleted;
    public $seen;
    public $draft;
    public $indices;
    public function __construct()
    {
        global $dictionary;

        if (!isset($dictionary['email_cache']) || empty($dictionary['email_cache'])) {
            if (file_exists('custom/metadata/email_cacheMetaData.php')) {
                include('custom/metadata/email_cacheMetaData.php');
            } else {
                include('metadata/email_cacheMetaData.php');
            }
        }

        $this->fieldDefs = $dictionary['email_cache']['fields'];
        $this->indices = $dictionary['email_cache']['indices'];
    }
}
