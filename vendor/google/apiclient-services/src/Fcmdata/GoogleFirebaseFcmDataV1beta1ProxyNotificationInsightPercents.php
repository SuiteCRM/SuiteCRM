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

namespace Google\Service\Fcmdata;

class GoogleFirebaseFcmDataV1beta1ProxyNotificationInsightPercents extends \Google\Model
{
  /**
   * @var float
   */
  public $failed;
  /**
   * @var float
   */
  public $proxied;
  /**
   * @var float
   */
  public $skippedNotThrottled;
  /**
   * @var float
   */
  public $skippedOptedOut;
  /**
   * @var float
   */
  public $skippedUnconfigured;
  /**
   * @var float
   */
  public $skippedUnsupported;

  /**
   * @param float
   */
  public function setFailed($failed)
  {
    $this->failed = $failed;
  }
  /**
   * @return float
   */
  public function getFailed()
  {
    return $this->failed;
  }
  /**
   * @param float
   */
  public function setProxied($proxied)
  {
    $this->proxied = $proxied;
  }
  /**
   * @return float
   */
  public function getProxied()
  {
    return $this->proxied;
  }
  /**
   * @param float
   */
  public function setSkippedNotThrottled($skippedNotThrottled)
  {
    $this->skippedNotThrottled = $skippedNotThrottled;
  }
  /**
   * @return float
   */
  public function getSkippedNotThrottled()
  {
    return $this->skippedNotThrottled;
  }
  /**
   * @param float
   */
  public function setSkippedOptedOut($skippedOptedOut)
  {
    $this->skippedOptedOut = $skippedOptedOut;
  }
  /**
   * @return float
   */
  public function getSkippedOptedOut()
  {
    return $this->skippedOptedOut;
  }
  /**
   * @param float
   */
  public function setSkippedUnconfigured($skippedUnconfigured)
  {
    $this->skippedUnconfigured = $skippedUnconfigured;
  }
  /**
   * @return float
   */
  public function getSkippedUnconfigured()
  {
    return $this->skippedUnconfigured;
  }
  /**
   * @param float
   */
  public function setSkippedUnsupported($skippedUnsupported)
  {
    $this->skippedUnsupported = $skippedUnsupported;
  }
  /**
   * @return float
   */
  public function getSkippedUnsupported()
  {
    return $this->skippedUnsupported;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleFirebaseFcmDataV1beta1ProxyNotificationInsightPercents::class, 'Google_Service_Fcmdata_GoogleFirebaseFcmDataV1beta1ProxyNotificationInsightPercents');
