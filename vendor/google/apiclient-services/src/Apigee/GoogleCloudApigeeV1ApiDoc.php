<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Apigee;

class GoogleCloudApigeeV1ApiDoc extends \Google\Collection
{
  protected $collection_key = 'categoryIds';
  /**
   * @var bool
   */
  public $anonAllowed;
  /**
   * @var string
   */
  public $apiProductName;
  /**
   * @var string[]
   */
  public $categoryIds;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $edgeAPIProductName;
  /**
   * @var string
   */
  public $graphqlEndpointUrl;
  /**
   * @var string
   */
  public $graphqlSchema;
  /**
   * @var string
   */
  public $graphqlSchemaDisplayName;
  /**
   * @var string
   */
  public $id;
  /**
   * @var string
   */
  public $imageUrl;
  /**
   * @var string
   */
  public $modified;
  /**
   * @var bool
   */
  public $published;
  /**
   * @var bool
   */
  public $requireCallbackUrl;
  /**
   * @var string
   */
  public $siteId;
  /**
   * @var string
   */
  public $specId;
  /**
   * @var string
   */
  public $title;
  /**
   * @var bool
   */
  public $visibility;

  /**
   * @param bool
   */
  public function setAnonAllowed($anonAllowed)
  {
    $this->anonAllowed = $anonAllowed;
  }
  /**
   * @return bool
   */
  public function getAnonAllowed()
  {
    return $this->anonAllowed;
  }
  /**
   * @param string
   */
  public function setApiProductName($apiProductName)
  {
    $this->apiProductName = $apiProductName;
  }
  /**
   * @return string
   */
  public function getApiProductName()
  {
    return $this->apiProductName;
  }
  /**
   * @param string[]
   */
  public function setCategoryIds($categoryIds)
  {
    $this->categoryIds = $categoryIds;
  }
  /**
   * @return string[]
   */
  public function getCategoryIds()
  {
    return $this->categoryIds;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setEdgeAPIProductName($edgeAPIProductName)
  {
    $this->edgeAPIProductName = $edgeAPIProductName;
  }
  /**
   * @return string
   */
  public function getEdgeAPIProductName()
  {
    return $this->edgeAPIProductName;
  }
  /**
   * @param string
   */
  public function setGraphqlEndpointUrl($graphqlEndpointUrl)
  {
    $this->graphqlEndpointUrl = $graphqlEndpointUrl;
  }
  /**
   * @return string
   */
  public function getGraphqlEndpointUrl()
  {
    return $this->graphqlEndpointUrl;
  }
  /**
   * @param string
   */
  public function setGraphqlSchema($graphqlSchema)
  {
    $this->graphqlSchema = $graphqlSchema;
  }
  /**
   * @return string
   */
  public function getGraphqlSchema()
  {
    return $this->graphqlSchema;
  }
  /**
   * @param string
   */
  public function setGraphqlSchemaDisplayName($graphqlSchemaDisplayName)
  {
    $this->graphqlSchemaDisplayName = $graphqlSchemaDisplayName;
  }
  /**
   * @return string
   */
  public function getGraphqlSchemaDisplayName()
  {
    return $this->graphqlSchemaDisplayName;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param string
   */
  public function setImageUrl($imageUrl)
  {
    $this->imageUrl = $imageUrl;
  }
  /**
   * @return string
   */
  public function getImageUrl()
  {
    return $this->imageUrl;
  }
  /**
   * @param string
   */
  public function setModified($modified)
  {
    $this->modified = $modified;
  }
  /**
   * @return string
   */
  public function getModified()
  {
    return $this->modified;
  }
  /**
   * @param bool
   */
  public function setPublished($published)
  {
    $this->published = $published;
  }
  /**
   * @return bool
   */
  public function getPublished()
  {
    return $this->published;
  }
  /**
   * @param bool
   */
  public function setRequireCallbackUrl($requireCallbackUrl)
  {
    $this->requireCallbackUrl = $requireCallbackUrl;
  }
  /**
   * @return bool
   */
  public function getRequireCallbackUrl()
  {
    return $this->requireCallbackUrl;
  }
  /**
   * @param string
   */
  public function setSiteId($siteId)
  {
    $this->siteId = $siteId;
  }
  /**
   * @return string
   */
  public function getSiteId()
  {
    return $this->siteId;
  }
  /**
   * @param string
   */
  public function setSpecId($specId)
  {
    $this->specId = $specId;
  }
  /**
   * @return string
   */
  public function getSpecId()
  {
    return $this->specId;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
  /**
   * @param bool
   */
  public function setVisibility($visibility)
  {
    $this->visibility = $visibility;
  }
  /**
   * @return bool
   */
  public function getVisibility()
  {
    return $this->visibility;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudApigeeV1ApiDoc::class, 'Google_Service_Apigee_GoogleCloudApigeeV1ApiDoc');
