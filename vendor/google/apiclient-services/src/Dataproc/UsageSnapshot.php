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

namespace Google\Service\Dataproc;

class UsageSnapshot extends \Google\Model
{
  /**
   * @var string
   */
  public $acceleratorType;
  /**
   * @var string
   */
  public $milliAccelerator;
  /**
   * @var string
   */
  public $milliDcu;
  /**
   * @var string
   */
  public $milliDcuPremium;
  /**
   * @var string
   */
  public $shuffleStorageGb;
  /**
   * @var string
   */
  public $shuffleStorageGbPremium;
  /**
   * @var string
   */
  public $snapshotTime;

  /**
   * @param string
   */
  public function setAcceleratorType($acceleratorType)
  {
    $this->acceleratorType = $acceleratorType;
  }
  /**
   * @return string
   */
  public function getAcceleratorType()
  {
    return $this->acceleratorType;
  }
  /**
   * @param string
   */
  public function setMilliAccelerator($milliAccelerator)
  {
    $this->milliAccelerator = $milliAccelerator;
  }
  /**
   * @return string
   */
  public function getMilliAccelerator()
  {
    return $this->milliAccelerator;
  }
  /**
   * @param string
   */
  public function setMilliDcu($milliDcu)
  {
    $this->milliDcu = $milliDcu;
  }
  /**
   * @return string
   */
  public function getMilliDcu()
  {
    return $this->milliDcu;
  }
  /**
   * @param string
   */
  public function setMilliDcuPremium($milliDcuPremium)
  {
    $this->milliDcuPremium = $milliDcuPremium;
  }
  /**
   * @return string
   */
  public function getMilliDcuPremium()
  {
    return $this->milliDcuPremium;
  }
  /**
   * @param string
   */
  public function setShuffleStorageGb($shuffleStorageGb)
  {
    $this->shuffleStorageGb = $shuffleStorageGb;
  }
  /**
   * @return string
   */
  public function getShuffleStorageGb()
  {
    return $this->shuffleStorageGb;
  }
  /**
   * @param string
   */
  public function setShuffleStorageGbPremium($shuffleStorageGbPremium)
  {
    $this->shuffleStorageGbPremium = $shuffleStorageGbPremium;
  }
  /**
   * @return string
   */
  public function getShuffleStorageGbPremium()
  {
    return $this->shuffleStorageGbPremium;
  }
  /**
   * @param string
   */
  public function setSnapshotTime($snapshotTime)
  {
    $this->snapshotTime = $snapshotTime;
  }
  /**
   * @return string
   */
  public function getSnapshotTime()
  {
    return $this->snapshotTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UsageSnapshot::class, 'Google_Service_Dataproc_UsageSnapshot');
