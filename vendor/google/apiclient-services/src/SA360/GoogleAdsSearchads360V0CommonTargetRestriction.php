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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0CommonTargetRestriction extends \Google\Model
{
  /**
   * @var bool
   */
  public $bidOnly;
  /**
   * @var string
   */
  public $targetingDimension;

  /**
   * @param bool
   */
  public function setBidOnly($bidOnly)
  {
    $this->bidOnly = $bidOnly;
  }
  /**
   * @return bool
   */
  public function getBidOnly()
  {
    return $this->bidOnly;
  }
  /**
   * @param string
   */
  public function setTargetingDimension($targetingDimension)
  {
    $this->targetingDimension = $targetingDimension;
  }
  /**
   * @return string
   */
  public function getTargetingDimension()
  {
    return $this->targetingDimension;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0CommonTargetRestriction::class, 'Google_Service_SA360_GoogleAdsSearchads360V0CommonTargetRestriction');
