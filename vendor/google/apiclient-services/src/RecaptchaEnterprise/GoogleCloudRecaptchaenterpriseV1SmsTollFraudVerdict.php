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

namespace Google\Service\RecaptchaEnterprise;

class GoogleCloudRecaptchaenterpriseV1SmsTollFraudVerdict extends \Google\Collection
{
  protected $collection_key = 'reasons';
  /**
   * @var string[]
   */
  public $reasons;
  /**
   * @var float
   */
  public $risk;

  /**
   * @param string[]
   */
  public function setReasons($reasons)
  {
    $this->reasons = $reasons;
  }
  /**
   * @return string[]
   */
  public function getReasons()
  {
    return $this->reasons;
  }
  /**
   * @param float
   */
  public function setRisk($risk)
  {
    $this->risk = $risk;
  }
  /**
   * @return float
   */
  public function getRisk()
  {
    return $this->risk;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudRecaptchaenterpriseV1SmsTollFraudVerdict::class, 'Google_Service_RecaptchaEnterprise_GoogleCloudRecaptchaenterpriseV1SmsTollFraudVerdict');
