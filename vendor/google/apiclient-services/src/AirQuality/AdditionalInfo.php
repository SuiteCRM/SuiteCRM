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

namespace Google\Service\AirQuality;

class AdditionalInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $effects;
  /**
   * @var string
   */
  public $sources;

  /**
   * @param string
   */
  public function setEffects($effects)
  {
    $this->effects = $effects;
  }
  /**
   * @return string
   */
  public function getEffects()
  {
    return $this->effects;
  }
  /**
   * @param string
   */
  public function setSources($sources)
  {
    $this->sources = $sources;
  }
  /**
   * @return string
   */
  public function getSources()
  {
    return $this->sources;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AdditionalInfo::class, 'Google_Service_AirQuality_AdditionalInfo');
