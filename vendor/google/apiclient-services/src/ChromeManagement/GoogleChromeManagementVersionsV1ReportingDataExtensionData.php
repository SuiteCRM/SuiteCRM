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

namespace Google\Service\ChromeManagement;

class GoogleChromeManagementVersionsV1ReportingDataExtensionData extends \Google\Collection
{
  protected $collection_key = 'permissions';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $extensionId;
  /**
   * @var string
   */
  public $extensionType;
  /**
   * @var string
   */
  public $homepageUri;
  /**
   * @var string
   */
  public $installationType;
  /**
   * @var bool
   */
  public $isDisabled;
  /**
   * @var bool
   */
  public $isWebstoreExtension;
  /**
   * @var int
   */
  public $manifestVersion;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $permissions;
  /**
   * @var string
   */
  public $version;

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
  public function setExtensionId($extensionId)
  {
    $this->extensionId = $extensionId;
  }
  /**
   * @return string
   */
  public function getExtensionId()
  {
    return $this->extensionId;
  }
  /**
   * @param string
   */
  public function setExtensionType($extensionType)
  {
    $this->extensionType = $extensionType;
  }
  /**
   * @return string
   */
  public function getExtensionType()
  {
    return $this->extensionType;
  }
  /**
   * @param string
   */
  public function setHomepageUri($homepageUri)
  {
    $this->homepageUri = $homepageUri;
  }
  /**
   * @return string
   */
  public function getHomepageUri()
  {
    return $this->homepageUri;
  }
  /**
   * @param string
   */
  public function setInstallationType($installationType)
  {
    $this->installationType = $installationType;
  }
  /**
   * @return string
   */
  public function getInstallationType()
  {
    return $this->installationType;
  }
  /**
   * @param bool
   */
  public function setIsDisabled($isDisabled)
  {
    $this->isDisabled = $isDisabled;
  }
  /**
   * @return bool
   */
  public function getIsDisabled()
  {
    return $this->isDisabled;
  }
  /**
   * @param bool
   */
  public function setIsWebstoreExtension($isWebstoreExtension)
  {
    $this->isWebstoreExtension = $isWebstoreExtension;
  }
  /**
   * @return bool
   */
  public function getIsWebstoreExtension()
  {
    return $this->isWebstoreExtension;
  }
  /**
   * @param int
   */
  public function setManifestVersion($manifestVersion)
  {
    $this->manifestVersion = $manifestVersion;
  }
  /**
   * @return int
   */
  public function getManifestVersion()
  {
    return $this->manifestVersion;
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
   * @param string[]
   */
  public function setPermissions($permissions)
  {
    $this->permissions = $permissions;
  }
  /**
   * @return string[]
   */
  public function getPermissions()
  {
    return $this->permissions;
  }
  /**
   * @param string
   */
  public function setVersion($version)
  {
    $this->version = $version;
  }
  /**
   * @return string
   */
  public function getVersion()
  {
    return $this->version;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleChromeManagementVersionsV1ReportingDataExtensionData::class, 'Google_Service_ChromeManagement_GoogleChromeManagementVersionsV1ReportingDataExtensionData');
