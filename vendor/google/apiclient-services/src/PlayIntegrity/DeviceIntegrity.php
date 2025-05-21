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

namespace Google\Service\PlayIntegrity;

class DeviceIntegrity extends \Google\Collection
{
  protected $collection_key = 'legacyDeviceRecognitionVerdict';
  protected $deviceAttributesType = DeviceAttributes::class;
  protected $deviceAttributesDataType = '';
  protected $deviceRecallType = DeviceRecall::class;
  protected $deviceRecallDataType = '';
  /**
   * @var string[]
   */
  public $deviceRecognitionVerdict;
  /**
   * @var string[]
   */
  public $legacyDeviceRecognitionVerdict;
  protected $recentDeviceActivityType = RecentDeviceActivity::class;
  protected $recentDeviceActivityDataType = '';

  /**
   * @param DeviceAttributes
   */
  public function setDeviceAttributes(DeviceAttributes $deviceAttributes)
  {
    $this->deviceAttributes = $deviceAttributes;
  }
  /**
   * @return DeviceAttributes
   */
  public function getDeviceAttributes()
  {
    return $this->deviceAttributes;
  }
  /**
   * @param DeviceRecall
   */
  public function setDeviceRecall(DeviceRecall $deviceRecall)
  {
    $this->deviceRecall = $deviceRecall;
  }
  /**
   * @return DeviceRecall
   */
  public function getDeviceRecall()
  {
    return $this->deviceRecall;
  }
  /**
   * @param string[]
   */
  public function setDeviceRecognitionVerdict($deviceRecognitionVerdict)
  {
    $this->deviceRecognitionVerdict = $deviceRecognitionVerdict;
  }
  /**
   * @return string[]
   */
  public function getDeviceRecognitionVerdict()
  {
    return $this->deviceRecognitionVerdict;
  }
  /**
   * @param string[]
   */
  public function setLegacyDeviceRecognitionVerdict($legacyDeviceRecognitionVerdict)
  {
    $this->legacyDeviceRecognitionVerdict = $legacyDeviceRecognitionVerdict;
  }
  /**
   * @return string[]
   */
  public function getLegacyDeviceRecognitionVerdict()
  {
    return $this->legacyDeviceRecognitionVerdict;
  }
  /**
   * @param RecentDeviceActivity
   */
  public function setRecentDeviceActivity(RecentDeviceActivity $recentDeviceActivity)
  {
    $this->recentDeviceActivity = $recentDeviceActivity;
  }
  /**
   * @return RecentDeviceActivity
   */
  public function getRecentDeviceActivity()
  {
    return $this->recentDeviceActivity;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DeviceIntegrity::class, 'Google_Service_PlayIntegrity_DeviceIntegrity');
