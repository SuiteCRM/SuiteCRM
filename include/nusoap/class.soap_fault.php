<?php

/*

Modification information for LGPL compliance

r57813 - 2010-08-19 10:34:44 -0700 (Thu, 19 Aug 2010) - kjing - Author: John Mertic <jmertic@sugarcrm.com>
    Bug 39085 - When loading the opposite search panel via ajax on the ListViews, call the index action instead of the ListView action to avoid touching pre-MVC code by accident.

r56990 - 2010-06-16 13:05:36 -0700 (Wed, 16 Jun 2010) - kjing - snapshot "Mango" svn branch to a new one for GitHub sync

r56989 - 2010-06-16 13:01:33 -0700 (Wed, 16 Jun 2010) - kjing - defunt "Mango" svn dev branch before github cutover

r55980 - 2010-04-19 13:31:28 -0700 (Mon, 19 Apr 2010) - kjing - create Mango (6.1) based on windex

r51719 - 2009-10-22 10:18:00 -0700 (Thu, 22 Oct 2009) - mitani - Converted to Build 3  tags and updated the build system 

r51634 - 2009-10-19 13:32:22 -0700 (Mon, 19 Oct 2009) - mitani - Windex is the branch for Sugar Sales 1.0 development

r50375 - 2009-08-24 18:07:43 -0700 (Mon, 24 Aug 2009) - dwong - branch kobe2 from tokyo r50372

r42807 - 2008-12-29 11:16:59 -0800 (Mon, 29 Dec 2008) - dwong - Branch from trunk/sugarcrm r42806 to branches/tokyo/sugarcrm

r13782 - 2006-06-06 10:58:55 -0700 (Tue, 06 Jun 2006) - majed - changes entry point code

r11115 - 2006-01-17 14:54:45 -0800 (Tue, 17 Jan 2006) - majed - add entry point validation

r8846 - 2005-10-31 11:01:12 -0800 (Mon, 31 Oct 2005) - majed - new version of nusoap

r5462 - 2005-05-25 13:50:11 -0700 (Wed, 25 May 2005) - majed - upgraded nusoap to .6.9

r573 - 2004-09-04 13:03:32 -0700 (Sat, 04 Sep 2004) - sugarclint - undoing copyrights added in inadvertantly.  --clint

r546 - 2004-09-03 11:49:38 -0700 (Fri, 03 Sep 2004) - sugarmsi - removed echo count

r354 - 2004-08-02 23:00:37 -0700 (Mon, 02 Aug 2004) - sugarjacob - Adding Soap


*/


if(!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');




/**
* Contains information for a SOAP fault.
* Mainly used for returning faults from deployed functions
* in a server instance.
* @author   Dietrich Ayala <dietrich@ganx4.com>

* @access public
*/
class nusoap_fault extends nusoap_base {
	/**
	 * The fault code (client|server)
	 * @var string
	 * @access private
	 */
	var $faultcode;
	/**
	 * The fault actor
	 * @var string
	 * @access private
	 */
	var $faultactor;
	/**
	 * The fault string, a description of the fault
	 * @var string
	 * @access private
	 */
	var $faultstring;
	/**
	 * The fault detail, typically a string or array of string
	 * @var mixed
	 * @access private
	 */
	var $faultdetail;

	/**
	* constructor
    *
    * @param string $faultcode (SOAP-ENV:Client | SOAP-ENV:Server)
    * @param string $faultactor only used when msg routed between multiple actors
    * @param string $faultstring human readable error message
    * @param mixed $faultdetail detail, typically a string or array of string
	*/
	function __construct($faultcode,$faultactor='',$faultstring='',$faultdetail=''){
        parent::__construct();
		$this->faultcode = $faultcode;
		$this->faultactor = $faultactor;
		$this->faultstring = $faultstring;
		$this->faultdetail = $faultdetail;
	}

	/**
	* serialize a fault
	*
	* @return	string	The serialization of the fault instance.
	* @access   public
	*/
	function serialize(){
		$ns_string = '';
		foreach($this->namespaces as $k => $v){
			$ns_string .= "\n  xmlns:$k=\"$v\"";
		}
		$return_msg =
			'<?xml version="1.0" encoding="'.$this->soap_defencoding.'"?>'.
			'<SOAP-ENV:Envelope SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"'.$ns_string.">\n".
				'<SOAP-ENV:Body>'.
				'<SOAP-ENV:Fault>'.
					$this->serialize_val($this->faultcode, 'faultcode').
					$this->serialize_val($this->faultactor, 'faultactor').
					$this->serialize_val($this->faultstring, 'faultstring').
					$this->serialize_val($this->faultdetail, 'detail').
				'</SOAP-ENV:Fault>'.
				'</SOAP-ENV:Body>'.
			'</SOAP-ENV:Envelope>';
		return $return_msg;
	}
}

/**
 * Backward compatibility
 */
class soap_fault extends nusoap_fault {
}


?>