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

namespace Google\Service\Backupdr;

class AttachedDisk extends \Google\Collection
{
  protected $collection_key = 'license';
  /**
   * @var bool
   */
  public $autoDelete;
  /**
   * @var bool
   */
  public $boot;
  /**
   * @var string
   */
  public $deviceName;
  protected $diskEncryptionKeyType = CustomerEncryptionKey::class;
  protected $diskEncryptionKeyDataType = '';
  /**
   * @var string
   */
  public $diskInterface;
  /**
   * @var string
   */
  public $diskSizeGb;
  /**
   * @var string
   */
  public $diskType;
  /**
   * @var string
   */
  public $diskTypeDeprecated;
  protected $guestOsFeatureType = GuestOsFeature::class;
  protected $guestOsFeatureDataType = 'array';
  /**
   * @var string
   */
  public $index;
  protected $initializeParamsType = InitializeParams::class;
  protected $initializeParamsDataType = '';
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string[]
   */
  public $license;
  /**
   * @var string
   */
  public $mode;
  /**
   * @var string
   */
  public $savedState;
  /**
   * @var string
   */
  public $source;
  /**
   * @var string
   */
  public $type;

  /**
   * @param bool
   */
  public function setAutoDelete($autoDelete)
  {
    $this->autoDelete = $autoDelete;
  }
  /**
   * @return bool
   */
  public function getAutoDelete()
  {
    return $this->autoDelete;
  }
  /**
   * @param bool
   */
  public function setBoot($boot)
  {
    $this->boot = $boot;
  }
  /**
   * @return bool
   */
  public function getBoot()
  {
    return $this->boot;
  }
  /**
   * @param string
   */
  public function setDeviceName($deviceName)
  {
    $this->deviceName = $deviceName;
  }
  /**
   * @return string
   */
  public function getDeviceName()
  {
    return $this->deviceName;
  }
  /**
   * @param CustomerEncryptionKey
   */
  public function setDiskEncryptionKey(CustomerEncryptionKey $diskEncryptionKey)
  {
    $this->diskEncryptionKey = $diskEncryptionKey;
  }
  /**
   * @return CustomerEncryptionKey
   */
  public function getDiskEncryptionKey()
  {
    return $this->diskEncryptionKey;
  }
  /**
   * @param string
   */
  public function setDiskInterface($diskInterface)
  {
    $this->diskInterface = $diskInterface;
  }
  /**
   * @return string
   */
  public function getDiskInterface()
  {
    return $this->diskInterface;
  }
  /**
   * @param string
   */
  public function setDiskSizeGb($diskSizeGb)
  {
    $this->diskSizeGb = $diskSizeGb;
  }
  /**
   * @return string
   */
  public function getDiskSizeGb()
  {
    return $this->diskSizeGb;
  }
  /**
   * @param string
   */
  public function setDiskType($diskType)
  {
    $this->diskType = $diskType;
  }
  /**
   * @return string
   */
  public function getDiskType()
  {
    return $this->diskType;
  }
  /**
   * @param string
   */
  public function setDiskTypeDeprecated($diskTypeDeprecated)
  {
    $this->diskTypeDeprecated = $diskTypeDeprecated;
  }
  /**
   * @return string
   */
  public function getDiskTypeDeprecated()
  {
    return $this->diskTypeDeprecated;
  }
  /**
   * @param GuestOsFeature[]
   */
  public function setGuestOsFeature($guestOsFeature)
  {
    $this->guestOsFeature = $guestOsFeature;
  }
  /**
   * @return GuestOsFeature[]
   */
  public function getGuestOsFeature()
  {
    return $this->guestOsFeature;
  }
  /**
   * @param string
   */
  public function setIndex($index)
  {
    $this->index = $index;
  }
  /**
   * @return string
   */
  public function getIndex()
  {
    return $this->index;
  }
  /**
   * @param InitializeParams
   */
  public function setInitializeParams(InitializeParams $initializeParams)
  {
    $this->initializeParams = $initializeParams;
  }
  /**
   * @return InitializeParams
   */
  public function getInitializeParams()
  {
    return $this->initializeParams;
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
   * @param string[]
   */
  public function setLicense($license)
  {
    $this->license = $license;
  }
  /**
   * @return string[]
   */
  public function getLicense()
  {
    return $this->license;
  }
  /**
   * @param string
   */
  public function setMode($mode)
  {
    $this->mode = $mode;
  }
  /**
   * @return string
   */
  public function getMode()
  {
    return $this->mode;
  }
  /**
   * @param string
   */
  public function setSavedState($savedState)
  {
    $this->savedState = $savedState;
  }
  /**
   * @return string
   */
  public function getSavedState()
  {
    return $this->savedState;
  }
  /**
   * @param string
   */
  public function setSource($source)
  {
    $this->source = $source;
  }
  /**
   * @return string
   */
  public function getSource()
  {
    return $this->source;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AttachedDisk::class, 'Google_Service_Backupdr_AttachedDisk');
