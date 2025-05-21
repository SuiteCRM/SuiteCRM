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

namespace Google\Service\FirebaseHosting;

class DnsUpdates extends \Google\Collection
{
  protected $collection_key = 'discovered';
  /**
   * @var string
   */
  public $checkTime;
  protected $desiredType = DnsRecordSet::class;
  protected $desiredDataType = 'array';
  protected $discoveredType = DnsRecordSet::class;
  protected $discoveredDataType = 'array';

  /**
   * @param string
   */
  public function setCheckTime($checkTime)
  {
    $this->checkTime = $checkTime;
  }
  /**
   * @return string
   */
  public function getCheckTime()
  {
    return $this->checkTime;
  }
  /**
   * @param DnsRecordSet[]
   */
  public function setDesired($desired)
  {
    $this->desired = $desired;
  }
  /**
   * @return DnsRecordSet[]
   */
  public function getDesired()
  {
    return $this->desired;
  }
  /**
   * @param DnsRecordSet[]
   */
  public function setDiscovered($discovered)
  {
    $this->discovered = $discovered;
  }
  /**
   * @return DnsRecordSet[]
   */
  public function getDiscovered()
  {
    return $this->discovered;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DnsUpdates::class, 'Google_Service_FirebaseHosting_DnsUpdates');
