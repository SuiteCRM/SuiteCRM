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

namespace Google\Service\Walletobjects;

class PassConstraints extends \Google\Collection
{
  protected $collection_key = 'nfcConstraint';
  /**
   * @var string[]
   */
  public $nfcConstraint;
  /**
   * @var string
   */
  public $screenshotEligibility;

  /**
   * @param string[]
   */
  public function setNfcConstraint($nfcConstraint)
  {
    $this->nfcConstraint = $nfcConstraint;
  }
  /**
   * @return string[]
   */
  public function getNfcConstraint()
  {
    return $this->nfcConstraint;
  }
  /**
   * @param string
   */
  public function setScreenshotEligibility($screenshotEligibility)
  {
    $this->screenshotEligibility = $screenshotEligibility;
  }
  /**
   * @return string
   */
  public function getScreenshotEligibility()
  {
    return $this->screenshotEligibility;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PassConstraints::class, 'Google_Service_Walletobjects_PassConstraints');
