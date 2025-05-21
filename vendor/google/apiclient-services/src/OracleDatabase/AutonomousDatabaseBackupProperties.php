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

namespace Google\Service\OracleDatabase;

class AutonomousDatabaseBackupProperties extends \Google\Model
{
  /**
   * @var string
   */
  public $availableTillTime;
  /**
   * @var string
   */
  public $compartmentId;
  /**
   * @var float
   */
  public $databaseSizeTb;
  /**
   * @var string
   */
  public $dbVersion;
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var bool
   */
  public $isAutomaticBackup;
  /**
   * @var bool
   */
  public $isLongTermBackup;
  /**
   * @var bool
   */
  public $isRestorable;
  /**
   * @var string
   */
  public $keyStoreId;
  /**
   * @var string
   */
  public $keyStoreWallet;
  /**
   * @var string
   */
  public $kmsKeyId;
  /**
   * @var string
   */
  public $kmsKeyVersionId;
  /**
   * @var string
   */
  public $lifecycleDetails;
  /**
   * @var string
   */
  public $lifecycleState;
  /**
   * @var string
   */
  public $ocid;
  /**
   * @var int
   */
  public $retentionPeriodDays;
  /**
   * @var float
   */
  public $sizeTb;
  /**
   * @var string
   */
  public $startTime;
  /**
   * @var string
   */
  public $type;
  /**
   * @var string
   */
  public $vaultId;

  /**
   * @param string
   */
  public function setAvailableTillTime($availableTillTime)
  {
    $this->availableTillTime = $availableTillTime;
  }
  /**
   * @return string
   */
  public function getAvailableTillTime()
  {
    return $this->availableTillTime;
  }
  /**
   * @param string
   */
  public function setCompartmentId($compartmentId)
  {
    $this->compartmentId = $compartmentId;
  }
  /**
   * @return string
   */
  public function getCompartmentId()
  {
    return $this->compartmentId;
  }
  /**
   * @param float
   */
  public function setDatabaseSizeTb($databaseSizeTb)
  {
    $this->databaseSizeTb = $databaseSizeTb;
  }
  /**
   * @return float
   */
  public function getDatabaseSizeTb()
  {
    return $this->databaseSizeTb;
  }
  /**
   * @param string
   */
  public function setDbVersion($dbVersion)
  {
    $this->dbVersion = $dbVersion;
  }
  /**
   * @return string
   */
  public function getDbVersion()
  {
    return $this->dbVersion;
  }
  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
  }
  /**
   * @param bool
   */
  public function setIsAutomaticBackup($isAutomaticBackup)
  {
    $this->isAutomaticBackup = $isAutomaticBackup;
  }
  /**
   * @return bool
   */
  public function getIsAutomaticBackup()
  {
    return $this->isAutomaticBackup;
  }
  /**
   * @param bool
   */
  public function setIsLongTermBackup($isLongTermBackup)
  {
    $this->isLongTermBackup = $isLongTermBackup;
  }
  /**
   * @return bool
   */
  public function getIsLongTermBackup()
  {
    return $this->isLongTermBackup;
  }
  /**
   * @param bool
   */
  public function setIsRestorable($isRestorable)
  {
    $this->isRestorable = $isRestorable;
  }
  /**
   * @return bool
   */
  public function getIsRestorable()
  {
    return $this->isRestorable;
  }
  /**
   * @param string
   */
  public function setKeyStoreId($keyStoreId)
  {
    $this->keyStoreId = $keyStoreId;
  }
  /**
   * @return string
   */
  public function getKeyStoreId()
  {
    return $this->keyStoreId;
  }
  /**
   * @param string
   */
  public function setKeyStoreWallet($keyStoreWallet)
  {
    $this->keyStoreWallet = $keyStoreWallet;
  }
  /**
   * @return string
   */
  public function getKeyStoreWallet()
  {
    return $this->keyStoreWallet;
  }
  /**
   * @param string
   */
  public function setKmsKeyId($kmsKeyId)
  {
    $this->kmsKeyId = $kmsKeyId;
  }
  /**
   * @return string
   */
  public function getKmsKeyId()
  {
    return $this->kmsKeyId;
  }
  /**
   * @param string
   */
  public function setKmsKeyVersionId($kmsKeyVersionId)
  {
    $this->kmsKeyVersionId = $kmsKeyVersionId;
  }
  /**
   * @return string
   */
  public function getKmsKeyVersionId()
  {
    return $this->kmsKeyVersionId;
  }
  /**
   * @param string
   */
  public function setLifecycleDetails($lifecycleDetails)
  {
    $this->lifecycleDetails = $lifecycleDetails;
  }
  /**
   * @return string
   */
  public function getLifecycleDetails()
  {
    return $this->lifecycleDetails;
  }
  /**
   * @param string
   */
  public function setLifecycleState($lifecycleState)
  {
    $this->lifecycleState = $lifecycleState;
  }
  /**
   * @return string
   */
  public function getLifecycleState()
  {
    return $this->lifecycleState;
  }
  /**
   * @param string
   */
  public function setOcid($ocid)
  {
    $this->ocid = $ocid;
  }
  /**
   * @return string
   */
  public function getOcid()
  {
    return $this->ocid;
  }
  /**
   * @param int
   */
  public function setRetentionPeriodDays($retentionPeriodDays)
  {
    $this->retentionPeriodDays = $retentionPeriodDays;
  }
  /**
   * @return int
   */
  public function getRetentionPeriodDays()
  {
    return $this->retentionPeriodDays;
  }
  /**
   * @param float
   */
  public function setSizeTb($sizeTb)
  {
    $this->sizeTb = $sizeTb;
  }
  /**
   * @return float
   */
  public function getSizeTb()
  {
    return $this->sizeTb;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
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
  /**
   * @param string
   */
  public function setVaultId($vaultId)
  {
    $this->vaultId = $vaultId;
  }
  /**
   * @return string
   */
  public function getVaultId()
  {
    return $this->vaultId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AutonomousDatabaseBackupProperties::class, 'Google_Service_OracleDatabase_AutonomousDatabaseBackupProperties');
