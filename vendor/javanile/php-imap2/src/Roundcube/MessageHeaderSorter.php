<?php


namespace Javanile\Imap2\Roundcube;
/**
 * Class for sorting an array of rcube_message_header objects in a predetermined order.
 *
 * @package    Framework
 * @subpackage Storage
 * @author  Aleksander Machniak <alec@alec.pl>
 */
class MessageHeaderSorter
{
    private $uids = array();


    /**
     * Set the predetermined sort order.
     *
     * @param array $index Numerically indexed array of IMAP UIDs
     */
    function set_index($index)
    {
        $index = array_flip($index);

        $this->uids = $index;
    }

    /**
     * Sort the array of header objects
     *
     * @param array $headers Array of rcube_message_header objects indexed by UID
     */
    function sort_headers(&$headers)
    {
        uksort($headers, array($this, "compare_uids"));
    }

    /**
     * Sort method called by uksort()
     *
     * @param int $a Array key (UID)
     * @param int $b Array key (UID)
     */
    function compare_uids($a, $b)
    {
// then find each sequence number in my ordered list
        $posa = isset($this->uids[$a]) ? intval($this->uids[$a]) : -1;
        $posb = isset($this->uids[$b]) ? intval($this->uids[$b]) : -1;

// return the relative position as the comparison value
        return $posa - $posb;
    }
}
