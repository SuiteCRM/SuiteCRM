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

namespace Google\Service\SQLAdmin;

class Backup extends \Google\Model
{
  protected $backupIntervalType = Interval::class;
  protected $backupIntervalDataType = '';
  /**
   * @var string
   */
  public $backupKind;
  /**
   * @var string
   */
  public $backupRun;
  /**
   * @var string
   */
  public $description;
  protected $errorType = OperationError::class;
  protected $errorDataType = '';
  /**
   * @var string
   */
  public $expiryTime;
  /**
   * @var string
   */
  public $instance;
  /**
   * @var string
   */
  public $kind;
  /**
   * @var string
   */
  public $kmsKey;
  /**
   * @var string
   */
  public $kmsKeyVersion;
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $maxChargeableBytes;
  /**
   * @var string
   */
  public $name;
  /**
   * @var bool
   */
  public $satisfiesPzi;
  /**
   * @var bool
   */
  public $satisfiesPzs;
  /**
   * @var string
   */
  public $selfLink;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $timeZone;
  /**
   * @var string
   */
  public $ttlDays;
  /**
   * @var string
   */
  public $type;

  /**
   * @param Interval
   */
  public function setBackupInterval(Interval $backupInterval)
  {
    $this->backupInterval = $backupInterval;
  }
  /**
   * @return Interval
   */
  public function getBackupInterval()
  {
    return $this->backupInterval;
  }
  /**
   * @param string
   */
  public function setBackupKind($backupKind)
  {
    $this->backupKind = $backupKind;
  }
  /**
   * @return string
   */
  public function getBackupKind()
  {
    return $this->backupKind;
  }
  /**
   * @param string
   */
  public function setBackupRun($backupRun)
  {
    $this->backupRun = $backupRun;
  }
  /**
   * @return string
   */
  public function getBackupRun()
  {
    return $this->backupRun;
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
   * @param OperationError
   */
  public function setError(OperationError $error)
  {
    $this->error = $error;
  }
  /**
   * @return OperationError
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param string
   */
  public function setExpiryTime($expiryTime)
  {
    $this->expiryTime = $expiryTime;
  }
  /**
   * @return string
   */
  public function getExpiryTime()
  {
    return $this->expiryTime;
  }
  /**
   * @param string
   */
  public function setInstance($instance)
  {
    $this->instance = $instance;
  }
  /**
   * @return string
   */
  public function getInstance()
  {
    return $this->instance;
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
  public function setKmsKey($kmsKey)
  {
    $this->kmsKey = $kmsKey;
  }
  /**
   * @return string
   */
  public function getKmsKey()
  {
    return $this->kmsKey;
  }
  /**
   * @param string
   */
  public function setKmsKeyVersion($kmsKeyVersion)
  {
    $this->kmsKeyVersion = $kmsKeyVersion;
  }
  /**
   * @return string
   */
  public function getKmsKeyVersion()
  {
    return $this->kmsKeyVersion;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string
   */
  public function setMaxChargeableBytes($maxChargeableBytes)
  {
    $this->maxChargeableBytes = $maxChargeableBytes;
  }
  /**
   * @return string
   */
  public function getMaxChargeableBytes()
  {
    return $this->maxChargeableBytes;
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
   * @param bool
   */
  public function setSatisfiesPzi($satisfiesPzi)
  {
    $this->satisfiesPzi = $satisfiesPzi;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzi()
  {
    return $this->satisfiesPzi;
  }
  /**
   * @param bool
   */
  public function setSatisfiesPzs($satisfiesPzs)
  {
    $this->satisfiesPzs = $satisfiesPzs;
  }
  /**
   * @return bool
   */
  public function getSatisfiesPzs()
  {
    return $this->satisfiesPzs;
  }
  /**
   * @param string
   */
  public function setSelfLink($selfLink)
  {
    $this->selfLink = $selfLink;
  }
  /**
   * @return string
   */
  public function getSelfLink()
  {
    return $this->selfLink;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
  /**
   * @param string
   */
  public function setTtlDays($ttlDays)
  {
    $this->ttlDays = $ttlDays;
  }
  /**
   * @return string
   */
  public function getTtlDays()
  {
    return $this->ttlDays;
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
class_alias(Backup::class, 'Google_Service_SQLAdmin_Backup');
