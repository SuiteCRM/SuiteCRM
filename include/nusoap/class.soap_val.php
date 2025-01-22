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
* For creating serializable abstractions of native PHP types.  This class
* allows element name/namespace, XSD type, and XML attributes to be
* associated with a value.  This is extremely useful when WSDL is not
* used, but is also useful when WSDL is used with polymorphic types, including
* xsd:anyType and user-defined types.
*
* @author   Dietrich Ayala <dietrich@ganx4.com>

* @access   public
*/
class soapval extends nusoap_base {
	/**
	 * The XML element name
	 *
	 * @var string
	 * @access private
	 */
	var $name;
	/**
	 * The XML type name (string or false)
	 *
	 * @var mixed
	 * @access private
	 */
	var $type;
	/**
	 * The PHP value
	 *
	 * @var mixed
	 * @access private
	 */
	var $value;
	/**
	 * The XML element namespace (string or false)
	 *
	 * @var mixed
	 * @access private
	 */
	var $element_ns;
	/**
	 * The XML type namespace (string or false)
	 *
	 * @var mixed
	 * @access private
	 */
	var $type_ns;
	/**
	 * The XML element attributes (array or false)
	 *
	 * @var mixed
	 * @access private
	 */
	var $attributes;

	/**
	* constructor
	*
	* @param    string $name optional name
	* @param    mixed $type optional type name
	* @param	mixed $value optional value
	* @param	mixed $element_ns optional namespace of value
	* @param	mixed $type_ns optional namespace of type
	* @param	mixed $attributes associative array of attributes to add to element serialization
	* @access   public
	*/
  	function __construct($name='soapval',$type=false,$value=-1,$element_ns=false,$type_ns=false,$attributes=false) {
		parent::__construct();
		$this->name = $name;
		$this->type = $type;
		$this->value = $value;
		$this->element_ns = $element_ns;
		$this->type_ns = $type_ns;
		$this->attributes = $attributes;
    }

	/**
	* return serialized value
	*
	* @param	string $use The WSDL use value (encoded|literal)
	* @return	string XML data
	* @access   public
	*/
	function serialize($use='encoded') {
		return $this->serialize_val($this->value, $this->name, $this->type, $this->element_ns, $this->type_ns, $this->attributes, $use, true);
    }

	/**
	* decodes a soapval object into a PHP native type
	*
	* @return	mixed
	* @access   public
	*/
	function decode(){
		return $this->value;
	}
}




?>