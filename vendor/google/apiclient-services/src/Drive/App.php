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

namespace Google\Service\Drive;

class App extends \Google\Collection
{
  protected $collection_key = 'secondaryMimeTypes';
  /**
   * @var bool
   */
  public $authorized;
  /**
   * @var string
   */
  public $createInFolderTemplate;
  /**
   * @var string
   */
  public $createUrl;
  /**
   * @var bool
   */
  public $hasDriveWideScope;
  protected $iconsType = AppIcons::class;
  protected $iconsDataType = 'array';
  /**
   * @var string
   */
  public $id;
  /**
   * @var bool
   */
  public $installed;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $longDescription;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $objectType;
  /**
   * @var string
   */
  public $openUrlTemplate;
  /**
   * @var string[]
   */
  public $primaryFileExtensions;
  /**
   * @var string[]
   */
  public $primaryMimeTypes;
  /**
   * @var string
   */
  public $productId;
  /**
   * @var string
   */
  public $productUrl;
  /**
   * @var string[]
   */
  public $secondaryFileExtensions;
  /**
   * @var string[]
   */
  public $secondaryMimeTypes;
  /**
   * @var string
   */
  public $shortDescription;
  /**
   * @var bool
   */
  public $supportsCreate;
  /**
   * @var bool
   */
  public $supportsImport;
  /**
   * @var bool
   */
  public $supportsMultiOpen;
  /**
   * @var bool
   */
  public $supportsOfflineCreate;
  /**
   * @var bool
   */
  public $useByDefault;

  /**
   * @param bool
   */
  public function setAuthorized($authorized)
  {
    $this->authorized = $authorized;
  }
  /**
   * @return bool
   */
  public function getAuthorized()
  {
    return $this->authorized;
  }
  /**
   * @param string
   */
  public function setCreateInFolderTemplate($createInFolderTemplate)
  {
    $this->createInFolderTemplate = $createInFolderTemplate;
  }
  /**
   * @return string
   */
  public function getCreateInFolderTemplate()
  {
    return $this->createInFolderTemplate;
  }
  /**
   * @param string
   */
  public function setCreateUrl($createUrl)
  {
    $this->createUrl = $createUrl;
  }
  /**
   * @return string
   */
  public function getCreateUrl()
  {
    return $this->createUrl;
  }
  /**
   * @param bool
   */
  public function setHasDriveWideScope($hasDriveWideScope)
  {
    $this->hasDriveWideScope = $hasDriveWideScope;
  }
  /**
   * @return bool
   */
  public function getHasDriveWideScope()
  {
    return $this->hasDriveWideScope;
  }
  /**
   * @param AppIcons[]
   */
  public function setIcons($icons)
  {
    $this->icons = $icons;
  }
  /**
   * @return AppIcons[]
   */
  public function getIcons()
  {
    return $this->icons;
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
   * @param bool
   */
  public function setInstalled($installed)
  {
    $this->installed = $installed;
  }
  /**
   * @return bool
   */
  public function getInstalled()
  {
    return $this->installed;
  }
  /**
   * @param string
   */
  public function setKind($kind)
  {
    $this->kind = $kind;
  }
  /**
   * @return string
   */
  public function getKind()
  {
    return $this->kind;
  }
  /**
   * @param string
   */
  public function setLongDescription($longDescription)
  {
    $this->longDescription = $longDescription;
  }
  /**
   * @return string
   */
  public function getLongDescription()
  {
    return $this->longDescription;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setObjectType($objectType)
  {
    $this->objectType = $objectType;
  }
  /**
   * @return string
   */
  public function getObjectType()
  {
    return $this->objectType;
  }
  /**
   * @param string
   */
  public function setOpenUrlTemplate($openUrlTemplate)
  {
    $this->openUrlTemplate = $openUrlTemplate;
  }
  /**
   * @return string
   */
  public function getOpenUrlTemplate()
  {
    return $this->openUrlTemplate;
  }
  /**
   * @param string[]
   */
  public function setPrimaryFileExtensions($primaryFileExtensions)
  {
    $this->primaryFileExtensions = $primaryFileExtensions;
  }
  /**
   * @return string[]
   */
  public function getPrimaryFileExtensions()
  {
    return $this->primaryFileExtensions;
  }
  /**
   * @param string[]
   */
  public function setPrimaryMimeTypes($primaryMimeTypes)
  {
    $this->primaryMimeTypes = $primaryMimeTypes;
  }
  /**
   * @return string[]
   */
  public function getPrimaryMimeTypes()
  {
    return $this->primaryMimeTypes;
  }
  /**
   * @param string
   */
  public function setProductId($productId)
  {
    $this->productId = $productId;
  }
  /**
   * @return string
   */
  public function getProductId()
  {
    return $this->productId;
  }
  /**
   * @param string
   */
  public function setProductUrl($productUrl)
  {
    $this->productUrl = $productUrl;
  }
  /**
   * @return string
   */
  public function getProductUrl()
  {
    return $this->productUrl;
  }
  /**
   * @param string[]
   */
  public function setSecondaryFileExtensions($secondaryFileExtensions)
  {
    $this->secondaryFileExtensions = $secondaryFileExtensions;
  }
  /**
   * @return string[]
   */
  public function getSecondaryFileExtensions()
  {
    return $this->secondaryFileExtensions;
  }
  /**
   * @param string[]
   */
  public function setSecondaryMimeTypes($secondaryMimeTypes)
  {
    $this->secondaryMimeTypes = $secondaryMimeTypes;
  }
  /**
   * @return string[]
   */
  public function getSecondaryMimeTypes()
  {
    return $this->secondaryMimeTypes;
  }
  /**
   * @param string
   */
  public function setShortDescription($shortDescription)
  {
    $this->shortDescription = $shortDescription;
  }
  /**
   * @return string
   */
  public function getShortDescription()
  {
    return $this->shortDescription;
  }
  /**
   * @param bool
   */
  public function setSupportsCreate($supportsCreate)
  {
    $this->supportsCreate = $supportsCreate;
  }
  /**
   * @return bool
   */
  public function getSupportsCreate()
  {
    return $this->supportsCreate;
  }
  /**
   * @param bool
   */
  public function setSupportsImport($supportsImport)
  {
    $this->supportsImport = $supportsImport;
  }
  /**
   * @return bool
   */
  public function getSupportsImport()
  {
    return $this->supportsImport;
  }
  /**
   * @param bool
   */
  public function setSupportsMultiOpen($supportsMultiOpen)
  {
    $this->supportsMultiOpen = $supportsMultiOpen;
  }
  /**
   * @return bool
   */
  public function getSupportsMultiOpen()
  {
    return $this->supportsMultiOpen;
  }
  /**
   * @param bool
   */
  public function setSupportsOfflineCreate($supportsOfflineCreate)
  {
    $this->supportsOfflineCreate = $supportsOfflineCreate;
  }
  /**
   * @return bool
   */
  public function getSupportsOfflineCreate()
  {
    return $this->supportsOfflineCreate;
  }
  /**
   * @param bool
   */
  public function setUseByDefault($useByDefault)
  {
    $this->useByDefault = $useByDefault;
  }
  /**
   * @return bool
   */
  public function getUseByDefault()
  {
    return $this->useByDefault;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(App::class, 'Google_Service_Drive_App');
