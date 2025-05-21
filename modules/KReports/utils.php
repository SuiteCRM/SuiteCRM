<?php
/**
 * This file is part of KReporter. KReporter is an enhancement developed
 * by Christian Knoll. All rights are (c) 2012 by Christian Knoll
 *
 * This file has been modified by SinergiaTIC in SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * You can contact Christian Knoll at info@kreporter.org
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

if(!function_exists("randomstring")){ 
    function randomstring(){
        $len = 10;
        $base='abcdefghjkmnpqrstwxyz';
        $max=strlen($base)-1;
        $returnstring = '';
        //2013-09-06 BUG #496 removed ... causing issues in higher php releases
        //mt_srand((double)microtime()*1000000);

        // STIC Custom 20230428 - JBL - Updated deprecated php syntax to access array elements
        // STIC#1064
        while (strlen($returnstring)<$len+1) {
            // $returnstring.=$base{mt_rand(0,$max)};
            $returnstring.=$base[mt_rand(0,$max)];
        }
        // End STIC Custom

        return $returnstring;

    }
}

if(!function_exists("json_decode_kinamu")){ 
    function json_decode_kinamu($json)
    { 
        if(function_exists('json_decode'))
            return json_decode($json, true);

        // bugfix 2010-8-23: problem with json in AJAX call
        if($json != '')
        {
            // Author: walidator.info 2009
            $comment = false;
            $out = '$x=';

            for ($i=0; $i<strlen($json); $i++)
            {
                if (!$comment)
                {
                    if ($json[$i] == '{' or $json[$i] == '[')        $out .= ' array(';
                    else if ($json[$i] == '}' or $json[$i] == ']')    $out .= ')';
                        else if ($json[$i] == ':')    $out .= '=>';
                            else                         $out .= $json[$i];           
                }
                else $out .= $json[$i];
                if ($json[$i] == '"')    $comment = !$comment;
            }
            eval($out . ';');
            return $x;
        }
        else 
        {
            return array();
        }
    }  
}

if(!function_exists("jarray_encode_kinamu")){ 
    function jarray_encode_kinamu($inArray)
    {
        if(!is_array($inArray))
            return '';

        // so we have an array
        foreach($inArray as $thisKey => $thisValue)
        {
            $resArray[] = "['" . $thisKey . "','" . $thisValue . "']"; 
        }
        return htmlentities('[' . implode(',', $resArray) . ']', ENT_QUOTES);
    }
}
if(!function_exists("json_encode_kinamu")){ 
    function json_encode_kinamu($input)
    {
        if(function_exists('json_encode'))
            // STIC-Custom 20211122 AAM - We need to specify this flag for encoding JSON, so the Unicode symbols are escaped properly in snapshots
            // STIC#488
            // return json_encode($input);
            return json_encode($input, JSON_UNESCAPED_UNICODE);
        else 
        {
            $json = new Services_JSON();
            return $json->encode($input);
        }
    }
}


// since this was moved with 5.5.1
if(!function_exists('html_entity_decode_utf8'))
{
    function html_entity_decode_utf8($string)
    {
        static $trans_tbl;
        // replace numeric entities
        $string = preg_replace('~&#x([0-9a-f]+);~ei', 'code2utf(hexdec("\\1"))', $string);
        $string = preg_replace('~&#([0-9]+);~e', 'code2utf(\\1)', $string);
        // replace literal entities
        if (!isset($trans_tbl))
        {
            $trans_tbl = array();
            foreach (get_html_translation_table(HTML_ENTITIES) as $val=>$key)
                // STIC Custom 20250304 JBL - Avoid Deprecated warning: Function utf8_encode() is deprecated since 8.2
                // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
                // $trans_tbl[$key] = utf8_encode($val);
                $trans_tbl[$key] = mb_convert_encoding($val, 'UTF-8', 'ISO-8859-1');
                // END STIC Custom
        }
        return strtr($string, $trans_tbl);
    }
}

function calculate_trendline($values, $offset = true)
{
    // get the total
    $sumX = 0; $sumY = 0;
    foreach($values as $datapointX => $datapointY)    
    {
        $sumY += $datapointY;
        $sumX += $datapointX;
    }

    // get the averages
    $avgX = $sumX / count($values);
    $avgY = $sumY / count($values);

    // get the alpha
    $sumNalpha = 0; $sumZalpha = 0;
    foreach($values as $datapointX => $datapointY)    
    {
        $sumNalpha += ($datapointX - $avgX)*($datapointY - $avgY);
        $sumZalpha += ($datapointX - $avgX) * ($datapointX - $avgX);
    }

    // calculate the alpha value
    $alpha = $sumZalpha > 0 ? $sumNalpha / $sumZalpha : 0;

    $startValue = $avgY - (((count($values) / 2) + 1) * $alpha); 
    $endValue = $avgY + (((count($values) / 2) + 1) * $alpha); 

    return array(
    'start' => round($startValue, 0), 
    'end' => round($endValue, 0)
    );
}
function multisort($array, $sort_by, $key1, $key2=NULL, $key3=NULL, $key4=NULL, $key5=NULL, $key6=NULL){
    // sort by ?
    foreach ($array as $pos =>  $val)
        $tmp_array[$pos] = $val[$sort_by];
    asort($tmp_array);

    // display however you want
    foreach ($tmp_array as $pos =>  $val){
        $return_array[$pos][$sort_by] = $array[$pos][$sort_by];
        $return_array[$pos][$key1] = $array[$pos][$key1];
        if (isset($key2)){
            $return_array[$pos][$key2] = $array[$pos][$key2];
        }
        if (isset($key3)){
            $return_array[$pos][$key3] = $array[$pos][$key3];
        }
        if (isset($key4)){
            $return_array[$pos][$key4] = $array[$pos][$key4];
        }
        if (isset($key5)){
            $return_array[$pos][$key5] = $array[$pos][$key5];
        }
        if (isset($key6)){
            $return_array[$pos][$key6] = $array[$pos][$key6];
        }
    }
    return $return_array;
}

function sortFieldArrayBySequence($first, $second)
{
    return $first['sequence'] - $second['sequence'];
}

function getLastDayOfMonth($month, $year) {
    return date('Y-m-d',strtotime('-1 second',strtotime('+1 month',strtotime($month.'/01/'.$year.' 00:00:00'))));
}
