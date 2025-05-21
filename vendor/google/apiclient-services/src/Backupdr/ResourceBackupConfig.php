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

class ResourceBackupConfig extends \Google\Collection
{
  protected $collection_key = 'backupConfigsDetails';
  protected $backupConfigsDetailsType = BackupConfigDetails::class;
  protected $backupConfigsDetailsDataType = 'array';
  /**
   * @var bool
   */
  public $backupConfigured;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $targetResource;
  /**
   * @var string
   */
  public $targetResourceDisplayName;
  /**
   * @var string[]
   */
  public $targetResourceLabels;
  /**
   * @var string
   */
  public $targetResourceType;
  /**
   * @var string
   */
  public $uid;
  /**
   * @var bool
   */
  public $vaulted;

  /**
   * @param BackupConfigDetails[]
   */
  public function setBackupConfigsDetails($backupConfigsDetails)
  {
    $this->backupConfigsDetails = $backupConfigsDetails;
  }
  /**
   * @return BackupConfigDetails[]
   */
  public function getBackupConfigsDetails()
  {
    return $this->backupConfigsDetails;
  }
  /**
   * @param bool
   */
  public function setBackupConfigured($backupConfigured)
  {
    $this->backupConfigured = $backupConfigured;
  }
  /**
   * @return bool
   */
  public function getBackupConfigured()
  {
    return $this->backupConfigured;
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
  public function setTargetResource($targetResource)
  {
    $this->targetResource = $targetResource;
  }
  /**
   * @return string
   */
  public function getTargetResource()
  {
    return $this->targetResource;
  }
  /**
   * @param string
   */
  public function setTargetResourceDisplayName($targetResourceDisplayName)
  {
    $this->targetResourceDisplayName = $targetResourceDisplayName;
  }
  /**
   * @return string
   */
  public function getTargetResourceDisplayName()
  {
    return $this->targetResourceDisplayName;
  }
  /**
   * @param string[]
   */
  public function setTargetResourceLabels($targetResourceLabels)
  {
    $this->targetResourceLabels = $targetResourceLabels;
  }
  /**
   * @return string[]
   */
  public function getTargetResourceLabels()
  {
    return $this->targetResourceLabels;
  }
  /**
   * @param string
   */
  public function setTargetResourceType($targetResourceType)
  {
    $this->targetResourceType = $targetResourceType;
  }
  /**
   * @return string
   */
  public function getTargetResourceType()
  {
    return $this->targetResourceType;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
  }
  /**
   * @param bool
   */
  public function setVaulted($vaulted)
  {
    $this->vaulted = $vaulted;
  }
  /**
   * @return bool
   */
  public function getVaulted()
  {
    return $this->vaulted;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ResourceBackupConfig::class, 'Google_Service_Backupdr_ResourceBackupConfig');
