<?php
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

define("HTTP_OK", 200);
define("HTTP_CREATED", 201);
define("HTTP_ACCEPTED", 202);
define("HTTP_NONAUTHORITATIVE_INFORMATION", 203);
define("HTTP_NO_CONTENT", 204);
define("HTTP_RESET_CONTENT", 205);
define("HTTP_PARTIAL_CONTENT", 206);
define("HTTP_MULTIPLE_CHOICES", 300);
define("HTTP_BAD_REQUEST", 400); // invalidArgument, filterNotValid
define("HTTP_UNAUTHORIZED", 401);
define("HTTP_FORBIDDEN", 403); // permissionDenied, streamNotSupported
define("HTTP_NOT_FOUND", 404); // objectNotFound
define("HTTP_METHOD_NOT_ALLOWED", 405); // notSupported
define("HTTP_NOT_ACCEPTABLE", 406);
define("HTTP_PROXY_AUTHENTICATION_REQUIRED", 407);
define("xHTTP_REQUEST_TIMEOUT", 408); //Had to change this b/c HTTP_REQUEST_TIMEOUT conflicts with definition in Drupal 7
define("HTTP_CONFLICT", 409); // constraint, contentAlreadyExists, versioning, updateConflict, nameConstraintViolation
define("HTTP_UNSUPPORTED_MEDIA_TYPE", 415);
define("HTTP_UNPROCESSABLE_ENTITY", 422);
define("HTTP_INTERNAL_SERVER_ERROR", 500); // runtime, storage

#[\AllowDynamicProperties]
class CmisInvalidArgumentException extends Exception
{
}
#[\AllowDynamicProperties]
class CmisObjectNotFoundException extends Exception
{
}
#[\AllowDynamicProperties]
class CmisPermissionDeniedException extends Exception
{
}
#[\AllowDynamicProperties]
class CmisNotSupportedException extends Exception
{
}
#[\AllowDynamicProperties]
class CmisConstraintException extends Exception
{
}
#[\AllowDynamicProperties]
class CmisRuntimeException extends Exception
{
}

#[\AllowDynamicProperties]
class CMISRepositoryWrapper
{
    // Handles --
    //   Workspace -- but only endpoints with a single repo
    //   Entry -- but only for objects
    //   Feeds -- but only for non-hierarchical feeds
    // Does not handle --
    //   -- Hierarchical Feeds
    //   -- Types
    //   -- Others?
    // Only Handles Basic Auth
    // Very Little Error Checking
    // Does not work against pre CMIS 1.0 Repos

    public $url;
    public $username;
    public $password;
    public $authenticated;
    public $workspace;
    public $last_request;
    public $do_not_urlencode;
    protected $_addlCurlOptions = array();

    public static $namespaces = array(
        "cmis" => "http://docs.oasis-open.org/ns/cmis/core/200908/",
        "cmisra" => "http://docs.oasis-open.org/ns/cmis/restatom/200908/",
        "atom" => "http://www.w3.org/2005/Atom",
        "app" => "http://www.w3.org/2007/app",

    );

    public function __construct($url, $username = null, $password = null, $options = null, array $addlCurlOptions = array())
    {
        if (is_array($options) && $options["config:do_not_urlencode"]) {
            $this->do_not_urlencode=true;
        }
        $this->_addlCurlOptions = $addlCurlOptions; // additional cURL options

        $this->connect($url, $username, $password, $options);
    }

    public static function getOpUrl($url, $options = null)
    {
        if (is_array($options) && (count($options) > 0)) {
            $needs_question = strstr((string) $url, "?") === false;
            return $url . ($needs_question ? "?" : "&") . http_build_query($options);
        } else {
            return $url;
        }
    }

    public function convertStatusCode($code, $message)
    {
        switch ($code) {
            case HTTP_BAD_REQUEST:
                return new CmisInvalidArgumentException($message, $code);
            case HTTP_NOT_FOUND:
                return new CmisObjectNotFoundException($message, $code);
            case HTTP_FORBIDDEN:
                return new CmisPermissionDeniedException($message, $code);
            case HTTP_METHOD_NOT_ALLOWED:
                return new CmisNotSupportedException($message, $code);
            case HTTP_CONFLICT:
                return new CmisConstraintException($message, $code);
            default:
                return new CmisRuntimeException($message, $code);
            }
    }

    public function connect($url, $username, $password, $options)
    {
        // TODO: Make this work with cookies
        $this->url = $url;
        $this->username = $username;
        $this->password = $password;
        $this->auth_options = $options;
        $this->authenticated = false;
        $retval = $this->doGet($this->url);
        if ($retval->code == HTTP_OK || $retval->code == HTTP_CREATED) {
            $this->authenticated = true;
            $this->workspace = CMISRepositoryWrapper :: extractWorkspace($retval->body);
        }
    }

    public function doGet($url)
    {
        $retval = $this->doRequest($url);
        if ($retval->code != HTTP_OK) {
            throw $this->convertStatusCode($retval->code, $retval->body);
        }
        return $retval;
    }

    public function doDelete($url)
    {
        $retval = $this->doRequest($url, "DELETE");
        if ($retval->code != HTTP_NO_CONTENT) {
            throw $this->convertStatusCode($retval->code, $retval->body);
        }
        return $retval;
    }

    public function doPost($url, $content, $contentType, $charset = null)
    {
        $retval = $this->doRequest($url, "POST", $content, $contentType);
        if ($retval->code != HTTP_CREATED) {
            throw $this->convertStatusCode($retval->code, $retval->body);
        }
        return $retval;
    }

    public function doPut($url, $content, $contentType, $charset = null)
    {
        $retval = $this->doRequest($url, "PUT", $content, $contentType);
        if (($retval->code < HTTP_OK) || ($retval->code >= HTTP_MULTIPLE_CHOICES)) {
            throw $this->convertStatusCode($retval->code, $retval->body);
        }
        return $retval;
    }

    public function doRequest($url, $method = "GET", $content = null, $contentType = null, $charset = null)
    {
        // Process the HTTP request
        // 'til now only the GET request has been tested
        // Does not URL encode any inputs yet
        if (is_array($this->auth_options)) {
            $url = CMISRepositoryWrapper :: getOpUrl($url, $this->auth_options);
        }
        $session = curl_init($url);
        curl_setopt($session, CURLOPT_HEADER, false);
        curl_setopt($session, CURLOPT_RETURNTRANSFER, true);
        if ($this->username) {
            curl_setopt($session, CURLOPT_USERPWD, $this->username . ":" . $this->password);
        }
        curl_setopt($session, CURLOPT_CUSTOMREQUEST, $method);
        if ($contentType) {
            curl_setopt($session, CURLOPT_HTTPHEADER, array(
                "Content-Type: " . $contentType
            ));
        }
        if ($content) {
            curl_setopt($session, CURLOPT_POSTFIELDS, $content);
        }
        if ($method == "POST") {
            curl_setopt($session, CURLOPT_POST, true);
        }

        // apply addl. cURL options
        // WARNING: this may override previously set options
        if (count($this->_addlCurlOptions)) {
            foreach ($this->_addlCurlOptions as $key => $value) {
                curl_setopt($session, $key, $value);
            }
        }


        //TODO: Make this storage optional
        $retval = new stdClass();
        $retval->url = $url;
        $retval->method = $method;
        $retval->content_sent = $content;
        $retval->content_type_sent = $contentType;
        $retval->body = curl_exec($session);
        $retval->code = curl_getinfo($session, CURLINFO_HTTP_CODE);
        $retval->content_type = curl_getinfo($session, CURLINFO_CONTENT_TYPE);
        $retval->content_length = curl_getinfo($session, CURLINFO_CONTENT_LENGTH_DOWNLOAD);
        curl_close($session);
        $this->last_request = $retval;
        return $retval;
    }

    public function getLastRequest()
    {
        return $this->last_request;
    }

    public function getLastRequestBody()
    {
        return $this->last_request->body;
    }

    public function getLastRequestCode()
    {
        return $this->last_request->code;
    }

    public function getLastRequestContentType()
    {
        return $this->last_request->content_type;
    }

    public function getLastRequestContentLength()
    {
        return $this->last_request->content_length;
    }

    public function getLastRequestURL()
    {
        return $this->last_request->url;
    }

    public function getLastRequestMethod()
    {
        return $this->last_request->method;
    }

    public function getLastRequestContentTypeSent()
    {
        return $this->last_request->content_type_sent;
    }

    public function getLastRequestContentSent()
    {
        return $this->last_request->content_sent;
    }

    // Static Utility Functions
    public static function processTemplate($template, $values = array())
    {
        // Fill in the blanks --
        $retval = $template;
        if (is_array($values)) {
            foreach ($values as $name => $value) {
                $retval = str_replace("{" . $name . "}", $value, (string) $retval);
            }
        }
        // Fill in any unpoupated variables with ""
        return preg_replace("/{[a-zA-Z0-9_]+}/", "", (string) $retval);
    }

    public static function doXQuery($xmldata, $xquery)
    {
        $doc = new DOMDocument();
        $doc->loadXML($xmldata);
        return CMISRepositoryWrapper :: doXQueryFromNode($doc, $xquery);
    }

    public static function doXQueryFromNode($xmlnode, $xquery)
    {
        // Perform an XQUERY on a NODE
        // Register the 4 CMIS namespaces
        //THis may be a hopeless HACK!
        //TODO: Review
        if (!($xmlnode instanceof DOMDocument)) {
            $xdoc=new DOMDocument();
            $xnode = $xdoc->importNode($xmlnode, true);
            $xdoc->appendChild($xnode);
            $xpath = new DomXPath($xdoc);
        } else {
            $xpath = new DomXPath($xmlnode);
        }
        foreach (CMISRepositoryWrapper :: $namespaces as $nspre => $nsuri) {
            $xpath->registerNamespace($nspre, $nsuri);
        }
        return $xpath->query($xquery);
    }
    public static function getLinksArray($xmlnode)
    {
        // Gets the links of an object or a workspace
        // Distinguishes between the two "down" links
        //  -- the children link is put into the associative array with the "down" index
        //  -- the descendants link is put into the associative array with the "down-tree" index
        //  These links are distinguished by the mime type attribute, but these are probably the only two links that share the same rel ..
        //    so this was done as a one off
        $links = array();
        $link_nodes = $xmlnode->getElementsByTagName("link");
        foreach ($link_nodes as $ln) {
            if ($ln->attributes->getNamedItem("rel")->nodeValue == "down" && $ln->attributes->getNamedItem("type")->nodeValue == "application/cmistree+xml") {
                //Descendents and children share same "rel" but different document type.
                $links["down-tree"] = $ln->attributes->getNamedItem("href")->nodeValue;
            } else {
                $links[$ln->attributes->getNamedItem("rel")->nodeValue] = $ln->attributes->getNamedItem("href")->nodeValue;
            }
        }
        return $links;
    }
    public static function extractAllowableActions($xmldata)
    {
        $doc = new DOMDocument();
        $doc->loadXML($xmldata);
        return CMISRepositoryWrapper :: extractAllowableActionsFromNode($doc);
    }
    public static function extractAllowableActionsFromNode($xmlnode)
    {
        $result = array();
        $allowableActions = $xmlnode->getElementsByTagName("allowableActions");
        if ($allowableActions->length > 0) {
            foreach ($allowableActions->item(0)->childNodes as $action) {
                if (isset($action->localName)) {
                    $result[$action->localName] = (preg_match("/^true$/i", (string) $action->nodeValue) > 0);
                }
            }
        }
        return $result;
    }
    public static function extractObject($xmldata)
    {
        $doc = new DOMDocument();
        $doc->loadXML($xmldata);
        return CMISRepositoryWrapper :: extractObjectFromNode($doc);
    }
    public static function extractObjectFromNode($xmlnode)
    {
        // Extracts the contents of an Object and organizes them into:
        //  -- Links
        //  -- Properties
        //  -- the Object ID
        // RRM -- NEED TO ADD ALLOWABLEACTIONS
        $retval = new stdClass();
        $retval->links = CMISRepositoryWrapper :: getLinksArray($xmlnode);
        $retval->properties = array();
        $prop_nodes = $xmlnode->getElementsByTagName("object")->item(0)->getElementsByTagName("properties")->item(0)->childNodes;
        foreach ($prop_nodes as $pn) {
            if ($pn->attributes) {
                $propDefId = $pn->attributes->getNamedItem("propertyDefinitionId");
                // TODO: Maybe use ->length=0 to even detect null values
                if (!is_null($propDefId) && $pn->getElementsByTagName("value") && $pn->getElementsByTagName("value")->item(0)) {
                    if ($pn->getElementsByTagName("value")->length > 1) {
                        $retval->properties[$propDefId->nodeValue] = array();
                        for ($idx=0;$idx < $pn->getElementsByTagName("value")->length;$idx++) {
                            $retval->properties[$propDefId->nodeValue][$idx] = $pn->getElementsByTagName("value")->item($idx)->nodeValue;
                        }
                    } else {
                        $retval->properties[$propDefId->nodeValue] = $pn->getElementsByTagName("value")->item(0)->nodeValue;
                    }
                }
            }
        }
        $retval->uuid = $xmlnode->getElementsByTagName("id")->item(0)->nodeValue;
        $retval->id = $retval->properties["cmis:objectId"];
        //TODO: RRM FIX THIS
        $children_node = $xmlnode->getElementsByTagName("children");
        $children_feed_c = null;
        if (is_object($children_node)) {
            $children_feed_c = $children_node->item(0);
        }
        if (is_object($children_feed_c)) {
            $children_feed_l = $children_feed_c->getElementsByTagName("feed");
        }
        if (isset($children_feed_l) && is_object($children_feed_l) && is_object($children_feed_l->item(0))) {
            $children_feed = $children_feed_l->item(0);
            $children_doc = new DOMDocument();
            $xnode = $children_doc->importNode($children_feed, true); // Avoid Wrong Document Error
            $children_doc->appendChild($xnode);
            $retval->children = CMISRepositoryWrapper :: extractObjectFeedFromNode($children_doc);
        }
        $retval->allowableActions = CMISRepositoryWrapper :: extractAllowableActionsFromNode($xmlnode);
        return $retval;
    }

    public function handleSpaces($path)
    {
        return $this->do_not_urlencode ? $path : rawurlencode($path);
    }

    public static function extractTypeDef($xmldata)
    {
        $doc = new DOMDocument();
        $doc->loadXML($xmldata);
        return CMISRepositoryWrapper :: extractTypeDefFromNode($doc);
    }
    public static function extractTypeDefFromNode($xmlnode)
    {
        // Extracts the contents of an Object and organizes them into:
        //  -- Links
        //  -- Properties
        //  -- the Object ID
        // RRM -- NEED TO ADD ALLOWABLEACTIONS
        $retval = new stdClass();
        $retval->links = CMISRepositoryWrapper :: getLinksArray($xmlnode);
        $retval->properties = array();
        $retval->attributes = array();
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "//cmisra:type/*");
        foreach ($result as $node) {
            if ((substr((string) $node->nodeName, 0, 13) == "cmis:property") && (substr((string) $node->nodeName, -10) == "Definition")) {
                $id = $node->getElementsByTagName("id")->item(0)->nodeValue;
                $cardinality = $node->getElementsByTagName("cardinality")->item(0)->nodeValue;
                $propertyType = $node->getElementsByTagName("propertyType")->item(0)->nodeValue;
                // Stop Gap for now
                $retval->properties[$id] = array(
                    "cmis:propertyType" => $propertyType,
                    "cmis:cardinality" => $cardinality,

                );
            } else {
                $retval->attributes[$node->nodeName] = $node->nodeValue;
            }
            $retval->id = $retval->attributes["cmis:id"];
        }
        //TODO: RRM FIX THIS
        $children_node = $xmlnode->getElementsByTagName("children");
        $children_feed_c = null;
        if (is_object($children_node)) {
            $children_feed_c = $children_node->item(0);
        }
        $children_feed_l = null;
        if (is_object($children_feed_c)) {
            $children_feed_l = $children_feed_c->getElementsByTagName("feed");
        }
        if (is_object($children_feed_l) && is_object($children_feed_l->item(0))) {
            $children_feed = $children_feed_l->item(0);
            $children_doc = new DOMDocument();
            $xnode = $children_doc->importNode($children_feed, true); // Avoid Wrong Document Error
            $children_doc->appendChild($xnode);
            $retval->children = CMISRepositoryWrapper :: extractTypeFeedFromNode($children_doc);
        }

        /*
         *



        		$prop_nodes = $xmlnode->getElementsByTagName("object")->item(0)->getElementsByTagName("properties")->item(0)->childNodes;
        		foreach ($prop_nodes as $pn) {
        			if ($pn->attributes) {
        				$retval->properties[$pn->attributes->getNamedItem("propertyDefinitionId")->nodeValue] = $pn->getElementsByTagName("value")->item(0)->nodeValue;
        			}
        		}
                $retval->uuid=$xmlnode->getElementsByTagName("id")->item(0)->nodeValue;
                $retval->id=$retval->properties["cmis:objectId"];
         */
        return $retval;
    }

    public static function extractObjectFeed($xmldata)
    {
        //Assumes only one workspace for now
        $doc = new DOMDocument();
        $doc->loadXML($xmldata);
        return CMISRepositoryWrapper :: extractObjectFeedFromNode($doc);
    }
    public static function extractObjectFeedFromNode($xmlnode)
    {
        // Process a feed and extract the objects
        //   Does not handle hierarchy
        //   Provides two arrays
        //   -- one sequential array (a list)
        //   -- one hash table indexed by objectID
        //   and a property "numItems" that holds the total number of items available.
        $retval = new stdClass();
        // extract total number of items
        $numItemsNode = CMISRepositoryWrapper::doXQueryFromNode($xmlnode, "/atom:feed/cmisra:numItems");
        $retval->numItems = $numItemsNode->length ? (int) $numItemsNode->item(0)->nodeValue : -1; // set to negative value if info is not available

        $retval->objectList = array();
        $retval->objectsById = array();
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "/atom:feed/atom:entry");
        foreach ($result as $node) {
            $obj = CMISRepositoryWrapper :: extractObjectFromNode($node);
            $retval->objectsById[$obj->id] = $obj;
            $retval->objectList[] = & $retval->objectsById[$obj->id];
        }
        return $retval;
    }

    public static function extractTypeFeed($xmldata)
    {
        //Assumes only one workspace for now
        $doc = new DOMDocument();
        $doc->loadXML($xmldata);
        return CMISRepositoryWrapper :: extractTypeFeedFromNode($doc);
    }
    public static function extractTypeFeedFromNode($xmlnode)
    {
        // Process a feed and extract the objects
        //   Does not handle hierarchy
        //   Provides two arrays
        //   -- one sequential array (a list)
        //   -- one hash table indexed by objectID
        $retval = new stdClass();
        $retval->objectList = array();
        $retval->objectsById = array();
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "/atom:feed/atom:entry");
        foreach ($result as $node) {
            $obj = CMISRepositoryWrapper :: extractTypeDefFromNode($node);
            $retval->objectsById[$obj->id] = $obj;
            $retval->objectList[] = & $retval->objectsById[$obj->id];
        }
        return $retval;
    }

    public static function extractWorkspace($xmldata)
    {
        //Assumes only one workspace for now
        $doc = new DOMDocument();
        $doc->loadXML($xmldata);
        return CMISRepositoryWrapper :: extractWorkspaceFromNode($doc);
    }
    public static function extractWorkspaceFromNode($xmlnode)
    {
        // Assumes only one workspace for now
        // Load up the workspace object with arrays of
        //  links
        //  URI Templates
        //  Collections
        //  Capabilities
        //  General Repository Information
        $retval = new stdClass();
        $retval->links = CMISRepositoryWrapper :: getLinksArray($xmlnode);
        $retval->uritemplates = array();
        $retval->collections = array();
        $retval->capabilities = array();
        $retval->repositoryInfo = array();
        $retval->permissions = array();
        $retval->permissionsMapping = array();
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "//cmisra:uritemplate");
        foreach ($result as $node) {
            $retval->uritemplates[$node->getElementsByTagName("type")->item(0)->nodeValue] = $node->getElementsByTagName("template")->item(0)->nodeValue;
        }
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "//app:collection");
        foreach ($result as $node) {
            $retval->collections[$node->getElementsByTagName("collectionType")->item(0)->nodeValue] = $node->attributes->getNamedItem("href")->nodeValue;
        }
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "//cmis:capabilities/*");
        foreach ($result as $node) {
            $retval->capabilities[$node->nodeName] = $node->nodeValue;
        }
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "//cmisra:repositoryInfo/*[name()!='cmis:capabilities' and name()!='cmis:aclCapability']");
        foreach ($result as $node) {
            $retval->repositoryInfo[$node->nodeName] = $node->nodeValue;
        }
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "//cmis:aclCapability/cmis:permissions");
        foreach ($result as $node) {
            $retval->permissions[$node->getElementsByTagName("permission")->item(0)->nodeValue] = $node->getElementsByTagName("description")->item(0)->nodeValue;
        }
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "//cmis:aclCapability/cmis:mapping");
        foreach ($result as $node) {
            $key = $node->getElementsByTagName("key")->item(0)->nodeValue;
            $values = array();
            foreach ($node->getElementsByTagName("permission") as $value) {
                array_push($values, $value->nodeValue);
            }
            $retval->permissionsMapping[$key] = $values;
        }
        $result = CMISRepositoryWrapper :: doXQueryFromNode($xmlnode, "//cmis:aclCapability/*[name()!='cmis:permissions' and name()!='cmis:mapping']");
        foreach ($result as $node) {
            $retval->repositoryInfo[$node->nodeName] = $node->nodeValue;
        }

        return $retval;
    }
}

// Option Contants for Array Indexing
// -- Generally optional flags that control how much information is returned
// -- Change log token is an anomoly -- but included in URL as parameter
define("OPT_MAX_ITEMS", "maxItems");
define("OPT_SKIP_COUNT", "skipCount");
define("OPT_FILTER", "filter");
define("OPT_INCLUDE_PROPERTY_DEFINITIONS", "includePropertyDefinitions");
define("OPT_INCLUDE_RELATIONSHIPS", "includeRelationships");
define("OPT_INCLUDE_POLICY_IDS", "includePolicyIds");
define("OPT_RENDITION_FILTER", "renditionFilter");
define("OPT_INCLUDE_ACL", "includeACL");
define("OPT_INCLUDE_ALLOWABLE_ACTIONS", "includeAllowableActions");
define("OPT_DEPTH", "depth");
define("OPT_CHANGE_LOG_TOKEN", "changeLogToken");

define("LINK_ALLOWABLE_ACTIONS", "http://docs.oasis-open.org/ns/cmis/link/200908/allowableactions");

define("MIME_ATOM_XML", 'application/atom+xml');
define("MIME_ATOM_XML_ENTRY", 'application/atom+xml;type=entry');
define("MIME_ATOM_XML_FEED", 'application/atom+xml;type=feed');
define("MIME_CMIS_TREE", 'application/cmistree+xml');
define("MIME_CMIS_QUERY", 'application/cmisquery+xml');

// Many Links have a pattern to them based upon objectId -- but can that be depended upon?

#[\AllowDynamicProperties]
class CMISService extends CMISRepositoryWrapper
{
    public $_link_cache;
    public $_title_cache;
    public $_objTypeId_cache;
    public $_type_cache;
    public function __construct($url, $username, $password, $options = null, array $addlCurlOptions = array())
    {
        parent :: __construct($url, $username, $password, $options, $addlCurlOptions);
        $this->_link_cache = array();
        $this->_title_cache = array();
        $this->_objTypeId_cache = array();
        $this->_type_cache = array();
    }

    // Utility Methods -- Added Titles
    // Should refactor to allow for single object
    public function cacheObjectInfo($obj)
    {
        $this->_link_cache[$obj->id] = $obj->links;
        $this->_title_cache[$obj->id] = $obj->properties["cmis:name"]; // Broad Assumption Here?
        $this->_objTypeId_cache[$obj->id] = $obj->properties["cmis:objectTypeId"];
    }

    public function cacheFeedInfo($objs)
    {
        foreach ($objs->objectList as $obj) {
            $this->cacheObjectInfo($obj);
        }
    }

    public function cacheTypeFeedInfo($typs)
    {
        foreach ($typs->objectList as $typ) {
            $this->cacheTypeInfo($typ);
        }
    }

    public function cacheTypeInfo($tDef)
    {
        // TODO: Fix Type Caching with missing properties
        $this->_type_cache[$tDef->id] = $tDef;
    }

    public function getPropertyType($typeId, $propertyId)
    {
        if ($this->_type_cache[$typeId]->properties) {
            return $this->_type_cache[$typeId]->properties[$propertyId]["cmis:propertyType"];
        }
        $obj = $this->getTypeDefinition($typeId);
        return $obj->properties[$propertyId]["cmis:propertyType"];
    }

    public function getObjectType($objectId)
    {
        if ($this->_objTypeId_cache[$objectId]) {
            return $this->_objTypeId_cache[$objectId];
        }
        $obj = $this->getObject($objectId);
        return $obj->properties["cmis:objectTypeId"];
    }

    public function getTitle($objectId)
    {
        if ($this->_title_cache[$objectId]) {
            return $this->_title_cache[$objectId];
        }
        $obj = $this->getObject($objectId);
        return $obj->properties["cmis:name"];
    }

    public function getTypeLink($typeId, $linkName)
    {
        if ($this->_type_cache[$typeId]->links) {
            return $this->_type_cache[$typeId]->links[$linkName];
        }
        $typ = $this->getTypeDefinition($typeId);
        return $typ->links[$linkName];
    }

    public function getLink($objectId, $linkName)
    {
        if ($this->_link_cache[$objectId][$linkName]) {
            return $this->_link_cache[$objectId][$linkName];
        }
        $obj = $this->getObject($objectId);
        return $obj->links[$linkName];
    }

    // Repository Services
    public function getRepositories()
    {
        throw Exception("Not Implemented");
    }

    public function getRepositoryInfo()
    {
        return $this->workspace;
    }

    public function getTypeDescendants($typeId=null, $depth, $options = array())
    {
        // TODO: Refactor Type Entries Caching
        $varmap = $options;
        if ($typeId) {
            $hash_values = $options;
            $hash_values['depth'] = $depth;
            $myURL = $this->getTypeLink($typeId, "down-tree");
            $myURL = CMISRepositoryWrapper :: getOpUrl($myURL, $hash_values);
        } else {
            $myURL = static::processTemplate($this->workspace->collections['http://docs.oasis-open.org/ns/cmis/link/200908/typedescendants'], $varmap);
        }
        $ret = $this->doGet($myURL);
        $typs = static::extractTypeFeed($ret->body);
        $this->cacheTypeFeedInfo($typs);
        return $typs;
    }

    public function getTypeChildren($typeId=null, $options = array())
    {
        // TODO: Refactor Type Entries Caching
        $varmap = $options;
        if ($typeId) {
            $myURL = $this->getTypeLink($typeId, "down");
        //TODO: Need GenURLQueryString Utility
        } else {
            //TODO: Need right URL
            $myURL = static::processTemplate($this->workspace->collections['types'], $varmap);
        }
        $ret = $this->doGet($myURL);
        $typs = static::extractTypeFeed($ret->body);
        $this->cacheTypeFeedInfo($typs);
        return $typs;
    }

    public function getTypeDefinition($typeId, $options = array())
    { // Nice to have
        $varmap = $options;
        $varmap["id"] = $typeId;
        $myURL = static::processTemplate($this->workspace->uritemplates['typebyid'], $varmap);
        $ret = $this->doGet($myURL);
        $obj = static::extractTypeDef($ret->body);
        $this->cacheTypeInfo($obj);
        return $obj;
    }

    public function getObjectTypeDefinition($objectId)
    { // Nice to have
        $myURL = $this->getLink($objectId, "describedby");
        $ret = $this->doGet($myURL);
        $obj = static::extractTypeDef($ret->body);
        $this->cacheTypeInfo($obj);
        return $obj;
    }
    //Navigation Services
    public function getFolderTree($folderId, $depth, $options = array())
    {
        $hash_values = $options;
        $hash_values['depth'] = $depth;
        $myURL = $this->getLink($folderId, "http://docs.oasis-open.org/ns/cmis/link/200908/foldertree");
        $myURL = CMISRepositoryWrapper :: getOpUrl($myURL, $hash_values);
        $ret = $this->doGet($myURL);
        $objs = static::extractObjectFeed($ret->body);
        $this->cacheFeedInfo($objs);
        return $objs;
    }

    public function getDescendants($folderId, $depth, $options = array())
    { // Nice to have
        $hash_values = $options;
        $hash_values['depth'] = $depth;
        $myURL = $this->getLink($folderId, "down-tree");
        $myURL = CMISRepositoryWrapper :: getOpUrl($myURL, $hash_values);
        $ret = $this->doGet($myURL);
        $objs = static::extractObjectFeed($ret->body);
        $this->cacheFeedInfo($objs);
        return $objs;
    }

    public function getChildren($folderId, $options = array())
    {
        $myURL = $this->getLink($folderId, "down");
        //TODO: Need GenURLQueryString Utility
        $ret = $this->doGet($myURL);
        $objs = static::extractObjectFeed($ret->body);
        $this->cacheFeedInfo($objs);
        return $objs;
    }

    public function getFolderParent($folderId, $options = array())
    { //yes
        $myURL = $this->getLink($folderId, "up");
        //TODO: Need GenURLQueryString Utility
        $ret = $this->doGet($myURL);
        $obj = $this->extractObjectEntry($ret->body);
        $this->cacheObjectInfo($obj);
        return $obj;
    }

    public function getObjectParents($objectId, $options = array())
    { // yes
        $myURL = $this->getLink($objectId, "up");
        //TODO: Need GenURLQueryString Utility
        $ret = $this->doGet($myURL);
        $objs = static::extractObjectFeed($ret->body);
        $this->cacheFeedInfo($objs);
        return $objs;
    }

    public function getCheckedOutDocs($options = array())
    {
        $obj_url = $this->workspace->collections['checkedout'];
        $ret = $this->doGet($obj_url);
        $objs = static::extractObjectFeed($ret->body);
        $this->cacheFeedInfo($objs);
        return $objs;
    }

    //Discovery Services

    public static function getQueryTemplate()
    {
        ob_start();
        echo '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' . "\n"; ?>
<cmis:query xmlns:cmis="http://docs.oasis-open.org/ns/cmis/core/200908/"
xmlns:cmism="http://docs.oasis-open.org/ns/cmis/messaging/200908/"
xmlns:atom="http://www.w3.org/2005/Atom"
xmlns:app="http://www.w3.org/2007/app"
xmlns:cmisra="http://docs.oasisopen.org/ns/cmis/restatom/200908/">
<cmis:statement>{q}</cmis:statement>
<cmis:searchAllVersions>{searchAllVersions}</cmis:searchAllVersions>
<cmis:includeAllowableActions>{includeAllowableActions}</cmis:includeAllowableActions>
<cmis:includeRelationships>{includeRelationships}</cmis:includeRelationships>
<cmis:renditionFilter>{renditionFilter}</cmis:renditionFilter>
<cmis:maxItems>{maxItems}</cmis:maxItems>
<cmis:skipCount>{skipCount}</cmis:skipCount>
</cmis:query>
<?php

        return ob_get_clean();
    }
    public function query($statement, $options = array())
    {
        static $query_template;
        if (!isset($query_template)) {
            $query_template = CMISService :: getQueryTemplate();
        }
        $hash_values = $options;
        $hash_values['q'] = $statement;
        $post_value = CMISRepositoryWrapper :: processTemplate($query_template, $hash_values);
        $ret = $this->doPost($this->workspace->collections['query'], $post_value, MIME_CMIS_QUERY);
        $objs = static::extractObjectFeed($ret->body);
        $this->cacheFeedInfo($objs);
        return $objs;
    }

    public function getContentChanges()
    {
        throw Exception("Not Implemented");
    }

    //Object Services
    public static function getEntryTemplate()
    {
        ob_start();
        echo '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' . "\n"; ?>
<atom:entry xmlns:cmis="http://docs.oasis-open.org/ns/cmis/core/200908/"
xmlns:cmism="http://docs.oasis-open.org/ns/cmis/messaging/200908/"
xmlns:atom="http://www.w3.org/2005/Atom"
xmlns:app="http://www.w3.org/2007/app"
xmlns:cmisra="http://docs.oasis-open.org/ns/cmis/restatom/200908/">
<atom:title>{title}</atom:title>
{SUMMARY}
{CONTENT}
<cmisra:object><cmis:properties>{PROPERTIES}</cmis:properties></cmisra:object>
</atom:entry>
<?php

        return ob_get_clean();
    }

    public static function getPropertyTemplate()
    {
        ob_start(); ?>
		<cmis:property{propertyType} propertyDefinitionId="{propertyId}">
			<cmis:value>{properties}</cmis:value>
		</cmis:property{propertyType}>
<?php

        return ob_get_clean();
    }

    public function processPropertyTemplates($objectType, $propMap)
    {
        static $propTemplate;
        static $propertyTypeMap;
        if (!isset($propTemplate)) {
            $propTemplate = CMISService :: getPropertyTemplate();
        }
        if (!isset($propertyTypeMap)) { // Not sure if I need to do this like this
            $propertyTypeMap = array(
                "integer" => "Integer",
                "boolean" => "Boolean",
                "datetime" => "DateTime",
                "decimal" => "Decimal",
                "html" => "Html",
                "id" => "Id",
                "string" => "String",
                "url" => "Url",
                "xml" => "Xml",

            );
        }
        $propertyContent = "";
        $hash_values = array();
        foreach ($propMap as $propId => $propValue) {
            $hash_values['propertyType'] = $propertyTypeMap[$this->getPropertyType($objectType, $propId)];
            $hash_values['propertyId'] = $propId;
            if (is_array($propValue)) {
                $first_one = true;
                $hash_values['properties'] = "";
                foreach ($propValue as $val) {
                    //This is a bit of a hack
                    if ($first_one) {
                        $first_one = false;
                    } else {
                        $hash_values['properties'] .= "</cmis:value>\n<cmis:value>";
                    }
                    $hash_values['properties'] .= $val;
                }
            } else {
                $hash_values['properties'] = $propValue;
            }
            //echo "HASH:\n";
            //print_r(array("template" =>$propTemplate, "Hash" => $hash_values));
            $propertyContent .= CMISRepositoryWrapper :: processTemplate($propTemplate, $hash_values);
        }
        return $propertyContent;
    }

    public static function getContentEntry($content, $content_type = "application/octet-stream")
    {
        static $contentTemplate;
        if (!isset($contentTemplate)) {
            $contentTemplate = CMISService :: getContentTemplate();
        }
        if ($content) {
            return CMISRepositoryWrapper :: processTemplate($contentTemplate, array(
                "content" => base64_encode(
                    $content
            ), "content_type" => $content_type));
        } else {
            return "";
        }
    }

    public static function getSummaryTemplate()
    {
        ob_start(); ?>
		<atom:summary>{summary}</atom:summary>
<?php

        return ob_get_clean();
    }

    public static function getContentTemplate()
    {
        ob_start(); ?>
		<cmisra:content>
			<cmisra:mediatype>
				{content_type}
			</cmisra:mediatype>
			<cmisra:base64>
				{content}
			</cmisra:base64>
		</cmisra:content>
<?php

        return ob_get_clean();
    }
    public static function createAtomEntry($name, $properties)
    {
    }
    public function getObject($objectId, $options = array())
    {
        $varmap = $options;
        $varmap["id"] = $objectId;
        $obj_url = static::processTemplate($this->workspace->uritemplates['objectbyid'], $varmap);
        $ret = $this->doGet($obj_url);
        $obj = static::extractObject($ret->body);
        $this->cacheObjectInfo($obj);
        return $obj;
    }

    public function getObjectByPath($path, $options = array())
    {
        $varmap = $options;
        $varmap["path"] = $this->handleSpaces($path);
        $obj_url = static::processTemplate($this->workspace->uritemplates['objectbypath'], $varmap);
        $ret = $this->doGet($obj_url);
        $obj = static::extractObject($ret->body);
        $this->cacheObjectInfo($obj);
        return $obj;
    }

    public function getProperties($objectId, $options = array())
    {
        // May need to set the options array default --
        return $this->getObject($objectId, $options);
    }

    public function getAllowableActions($objectId, $options = array())
    {
        $myURL = $this->getLink($objectId, LINK_ALLOWABLE_ACTIONS);
        $ret = $this->doGet($myURL);
        $result = static::extractAllowableActions($ret->body);
        return $result;
    }

    public function getRenditions($objectId, $options = array(
        OPT_RENDITION_FILTER => "*"
    ))
    {
        return getObject($objectId, $options);
    }

    public function getContentStream($objectId, $options = array())
    { // Yes
        $myURL = $this->getLink($objectId, "edit-media");
        $ret = $this->doGet($myURL);
        // doRequest stores the last request information in this object
        return $ret->body;
    }

    public function postObject($folderId, $objectName, $objectType, $properties = array(), $content = null, $content_type = "application/octet-stream", $options = array())
    { // Yes
        $myURL = $this->getLink($folderId, "down");
        // TODO: Need Proper Query String Handling
        // Assumes that the 'down' link does not have a querystring in it
        $myURL = CMISRepositoryWrapper :: getOpUrl($myURL, $options);
        static $entry_template;
        if (!isset($entry_template)) {
            $entry_template = CMISService :: getEntryTemplate();
        }
        if (is_array($properties)) {
            $hash_values = $properties;
        } else {
            $hash_values = array();
        }
        if (!isset($hash_values["cmis:objectTypeId"])) {
            $hash_values["cmis:objectTypeId"] = $objectType;
        }
        $properties_xml = $this->processPropertyTemplates($hash_values["cmis:objectTypeId"], $hash_values);
        if (is_array($options)) {
            $hash_values = $options;
        } else {
            $hash_values = array();
        }
        $hash_values["PROPERTIES"] = $properties_xml;
        $hash_values["SUMMARY"] = CMISService :: getSummaryTemplate();
        if ($content) {
            $hash_values["CONTENT"] = CMISService :: getContentEntry($content, $content_type);
        }
        if (!isset($hash_values['title'])) {
            $hash_values['title'] = $objectName;
        }
        if (!isset($hash_values['summary'])) {
            $hash_values['summary'] = $objectName;
        }
        $post_value = CMISRepositoryWrapper :: processTemplate($entry_template, $hash_values);
        $ret = $this->doPost($myURL, $post_value, MIME_ATOM_XML_ENTRY);
        // print "DO_POST\n";
        // print_r($ret);
        $obj = static::extractObject($ret->body);
        $this->cacheObjectInfo($obj);
        return $obj;
    }

    public function createDocument($folderId, $fileName, $properties = array(), $content = null, $content_type = "application/octet-stream", $options = array())
    { // Yes
        return $this->postObject($folderId, $fileName, "cmis:document", $properties, $content, $content_type, $options);
    }

    public function createDocumentFromSource()
    { //Yes?
        throw new CmisNotSupportedException("createDocumentFromSource is not supported by the AtomPub binding!");
    }

    public function createFolder($folderId, $folderName, $properties = array(), $options = array())
    { // Yes
        return $this->postObject($folderId, $folderName, "cmis:folder", $properties, null, null, $options);
    }

    public function createRelationship()
    { // Not in first Release
        throw Exception("Not Implemented");
    }

    public function createPolicy()
    { // Not in first Release
        throw Exception("Not Implemented");
    }

    public function updateProperties($objectId, $properties = array(), $options = array())
    { // Yes
        $varmap = $options;
        $varmap["id"] = $objectId;
        $objectName = $this->getTitle($objectId);
        $objectType = $this->getObjectType($objectId);
        $obj_url = $this->getLink($objectId, "edit");
        $obj_url = CMISRepositoryWrapper :: getOpUrl($obj_url, $options);
        static $entry_template;
        if (!isset($entry_template)) {
            $entry_template = CMISService :: getEntryTemplate();
        }
        if (is_array($properties)) {
            $hash_values = $properties;
        } else {
            $hash_values = array();
        }
        $properties_xml = $this->processPropertyTemplates($objectType, $hash_values);
        if (is_array($options)) {
            $hash_values = $options;
        } else {
            $hash_values = array();
        }
        $hash_values["PROPERTIES"] = $properties_xml;
        $hash_values["SUMMARY"] = CMISService :: getSummaryTemplate();
        if (!isset($hash_values['title'])) {
            $hash_values['title'] = $objectName;
        }
        if (!isset($hash_values['summary'])) {
            $hash_values['summary'] = $objectName;
        }
        $put_value = CMISRepositoryWrapper :: processTemplate($entry_template, $hash_values);
        // print $put_value; // RRM DEBUG
        $ret = $this->doPut($obj_url, $put_value, MIME_ATOM_XML_ENTRY);
        $obj = static::extractObject($ret->body);
        $this->cacheObjectInfo($obj);
        return $obj;
    }

    public function moveObject($objectId, $targetFolderId, $sourceFolderId, $options = array())
    { //yes
        $options['sourceFolderId'] = $sourceFolderId;
        return $this->postObject($targetFolderId, $this->getTitle($objectId), $this->getObjectType($objectId), array(
            "cmis:objectId" => $objectId
        ), null, null, $options);
    }

    public function deleteObject($objectId, $options = array())
    { //Yes
        $varmap = $options;
        $varmap["id"] = $objectId;
        $obj_url = $this->getLink($objectId, "edit");
        $ret = $this->doDelete($obj_url);
        return;
    }

    public function deleteTree()
    { // Nice to have
        throw Exception("Not Implemented");
    }

    public function setContentStream($objectId, $content, $content_type, $options = array())
    { //Yes
        $myURL = $this->getLink($objectId, "edit-media");
        $ret = $this->doPut($myURL, $content, $content_type);
    }

    public function deleteContentStream($objectId, $options = array())
    { //yes
        $myURL = $this->getLink($objectId, "edit-media");
        $ret = $this->doDelete($myURL);
        return;
    }

    //Versioning Services
    public function getPropertiesOfLatestVersion($objectId, $major =false, $options = array())
    {
        return $this->getObjectOfLatestVersion($objectId, $major, $options);
    }

    public function getObjectOfLatestVersion($objectId, $major = false, $options = array())
    {
        return $this->getObject($objectId, $options); // Won't be able to handle major/minor distinction
        // Need to add this -- "current-version"
        /*
         * Headers: CMIS-filter, CMIS-returnVersion (enumReturnVersion)
         * HTTP Arguments: filter, returnVersion
         * Enum returnVersion: This, Latest, Major
         */
    }

    public function getAllVersions()
    {
        throw Exception("Not Implemented");
    }

    public function checkOut()
    {
        throw Exception("Not Implemented");
    }

    public function checkIn()
    {
        throw Exception("Not Implemented");
    }

    public function cancelCheckOut()
    {
        throw Exception("Not Implemented");
    }

    public function deleteAllVersions()
    {
        throw Exception("Not Implemented");
    }

    //Relationship Services
    public function getObjectRelationships()
    {
        // get stripped down version of object (for the links) and then get the relationships?
        // Low priority -- can get all information when getting object
        throw Exception("Not Implemented");
    }

    //Multi-Filing Services
    public function addObjectToFolder()
    { // Probably
        throw Exception("Not Implemented");
    }

    public function removeObjectFromFolder()
    { //Probably
        throw Exception("Not Implemented");
    }

    //Policy Services
    public function getAppliedPolicies()
    {
        throw Exception("Not Implemented");
    }

    public function applyPolicy()
    {
        throw Exception("Not Implemented");
    }

    public function removePolicy()
    {
        throw Exception("Not Implemented");
    }

    //ACL Services
    public function getACL()
    {
        throw Exception("Not Implemented");
    }

    public function applyACL()
    {
        throw Exception("Not Implemented");
    }
}
