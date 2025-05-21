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

namespace Google\Service\AndroidPublisher;

class UpdateSubscriptionRequest extends \Google\Model
{
  /**
   * @var bool
   */
  public $allowMissing;
  /**
   * @var string
   */
  public $latencyTolerance;
  protected $regionsVersionType = RegionsVersion::class;
  protected $regionsVersionDataType = '';
  protected $subscriptionType = Subscription::class;
  protected $subscriptionDataType = '';
  /**
   * @var string
   */
  public $updateMask;

  /**
   * @param bool
   */
  public function setAllowMissing($allowMissing)
  {
    $this->allowMissing = $allowMissing;
  }
  /**
   * @return bool
   */
  public function getAllowMissing()
  {
    return $this->allowMissing;
  }
  /**
   * @param string
   */
  public function setLatencyTolerance($latencyTolerance)
  {
    $this->latencyTolerance = $latencyTolerance;
  }
  /**
   * @return string
   */
  public function getLatencyTolerance()
  {
    return $this->latencyTolerance;
  }
  /**
   * @param RegionsVersion
   */
  public function setRegionsVersion(RegionsVersion $regionsVersion)
  {
    $this->regionsVersion = $regionsVersion;
  }
  /**
   * @return RegionsVersion
   */
  public function getRegionsVersion()
  {
    return $this->regionsVersion;
  }
  /**
   * @param Subscription
   */
  public function setSubscription(Subscription $subscription)
  {
    $this->subscription = $subscription;
  }
  /**
   * @return Subscription
   */
  public function getSubscription()
  {
    return $this->subscription;
  }
  /**
   * @param string
   */
  public function setUpdateMask($updateMask)
  {
    $this->updateMask = $updateMask;
  }
  /**
   * @return string
   */
  public function getUpdateMask()
  {
    return $this->updateMask;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UpdateSubscriptionRequest::class, 'Google_Service_AndroidPublisher_UpdateSubscriptionRequest');
