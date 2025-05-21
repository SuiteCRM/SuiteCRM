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

namespace Google\Service\NetAppFiles;

class Backup extends \Google\Model
{
  /**
   * @var string
   */
  public $backupType;
  /**
   * @var string
   */
  public $chainStorageBytes;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string[]
   */
  public $labels;
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
  public $sourceSnapshot;
  /**
   * @var string
   */
  public $sourceVolume;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $volumeUsageBytes;

  /**
   * @param string
   */
  public function setBackupType($backupType)
  {
    $this->backupType = $backupType;
  }
  /**
   * @return string
   */
  public function getBackupType()
  {
    return $this->backupType;
  }
  /**
   * @param string
   */
  public function setChainStorageBytes($chainStorageBytes)
  {
    $this->chainStorageBytes = $chainStorageBytes;
  }
  /**
   * @return string
   */
  public function getChainStorageBytes()
  {
    return $this->chainStorageBytes;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
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
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
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
  public function setSourceSnapshot($sourceSnapshot)
  {
    $this->sourceSnapshot = $sourceSnapshot;
  }
  /**
   * @return string
   */
  public function getSourceSnapshot()
  {
    return $this->sourceSnapshot;
  }
  /**
   * @param string
   */
  public function setSourceVolume($sourceVolume)
  {
    $this->sourceVolume = $sourceVolume;
  }
  /**
   * @return string
   */
  public function getSourceVolume()
  {
    return $this->sourceVolume;
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
  public function setVolumeUsageBytes($volumeUsageBytes)
  {
    $this->volumeUsageBytes = $volumeUsageBytes;
  }
  /**
   * @return string
   */
  public function getVolumeUsageBytes()
  {
    return $this->volumeUsageBytes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Backup::class, 'Google_Service_NetAppFiles_Backup');
