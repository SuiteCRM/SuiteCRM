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

namespace Google\Service\AndroidProvisioningPartner;

class ClaimDeviceRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $configurationId;
  /**
   * @var string
   */
  public $customerId;
  protected $deviceIdentifierType = DeviceIdentifier::class;
  protected $deviceIdentifierDataType = '';
  protected $deviceMetadataType = DeviceMetadata::class;
  protected $deviceMetadataDataType = '';
  /**
   * @var string
   */
  public $googleWorkspaceCustomerId;
  /**
   * @var string
   */
  public $preProvisioningToken;
  /**
   * @var string
   */
  public $sectionType;
  /**
   * @var string
   */
  public $simlockProfileId;

  /**
   * @param string
   */
  public function setConfigurationId($configurationId)
  {
    $this->configurationId = $configurationId;
  }
  /**
   * @return string
   */
  public function getConfigurationId()
  {
    return $this->configurationId;
  }
  /**
   * @param string
   */
  public function setCustomerId($customerId)
  {
    $this->customerId = $customerId;
  }
  /**
   * @return string
   */
  public function getCustomerId()
  {
    return $this->customerId;
  }
  /**
   * @param DeviceIdentifier
   */
  public function setDeviceIdentifier(DeviceIdentifier $deviceIdentifier)
  {
    $this->deviceIdentifier = $deviceIdentifier;
  }
  /**
   * @return DeviceIdentifier
   */
  public function getDeviceIdentifier()
  {
    return $this->deviceIdentifier;
  }
  /**
   * @param DeviceMetadata
   */
  public function setDeviceMetadata(DeviceMetadata $deviceMetadata)
  {
    $this->deviceMetadata = $deviceMetadata;
  }
  /**
   * @return DeviceMetadata
   */
  public function getDeviceMetadata()
  {
    return $this->deviceMetadata;
  }
  /**
   * @param string
   */
  public function setGoogleWorkspaceCustomerId($googleWorkspaceCustomerId)
  {
    $this->googleWorkspaceCustomerId = $googleWorkspaceCustomerId;
  }
  /**
   * @return string
   */
  public function getGoogleWorkspaceCustomerId()
  {
    return $this->googleWorkspaceCustomerId;
  }
  /**
   * @param string
   */
  public function setPreProvisioningToken($preProvisioningToken)
  {
    $this->preProvisioningToken = $preProvisioningToken;
  }
  /**
   * @return string
   */
  public function getPreProvisioningToken()
  {
    return $this->preProvisioningToken;
  }
  /**
   * @param string
   */
  public function setSectionType($sectionType)
  {
    $this->sectionType = $sectionType;
  }
  /**
   * @return string
   */
  public function getSectionType()
  {
    return $this->sectionType;
  }
  /**
   * @param string
   */
  public function setSimlockProfileId($simlockProfileId)
  {
    $this->simlockProfileId = $simlockProfileId;
  }
  /**
   * @return string
   */
  public function getSimlockProfileId()
  {
    return $this->simlockProfileId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ClaimDeviceRequest::class, 'Google_Service_AndroidProvisioningPartner_ClaimDeviceRequest');
