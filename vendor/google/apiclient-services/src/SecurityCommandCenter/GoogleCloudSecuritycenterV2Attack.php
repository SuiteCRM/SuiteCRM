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

namespace Google\Service\SecurityCommandCenter;

class GoogleCloudSecuritycenterV2Attack extends \Google\Model
{
  /**
   * @var string
   */
  public $classification;
  /**
   * @var int
   */
  public $volumeBps;
  /**
   * @var string
   */
  public $volumeBpsLong;
  /**
   * @var int
   */
  public $volumePps;
  /**
   * @var string
   */
  public $volumePpsLong;

  /**
   * @param string
   */
  public function setClassification($classification)
  {
    $this->classification = $classification;
  }
  /**
   * @return string
   */
  public function getClassification()
  {
    return $this->classification;
  }
  /**
   * @param int
   */
  public function setVolumeBps($volumeBps)
  {
    $this->volumeBps = $volumeBps;
  }
  /**
   * @return int
   */
  public function getVolumeBps()
  {
    return $this->volumeBps;
  }
  /**
   * @param string
   */
  public function setVolumeBpsLong($volumeBpsLong)
  {
    $this->volumeBpsLong = $volumeBpsLong;
  }
  /**
   * @return string
   */
  public function getVolumeBpsLong()
  {
    return $this->volumeBpsLong;
  }
  /**
   * @param int
   */
  public function setVolumePps($volumePps)
  {
    $this->volumePps = $volumePps;
  }
  /**
   * @return int
   */
  public function getVolumePps()
  {
    return $this->volumePps;
  }
  /**
   * @param string
   */
  public function setVolumePpsLong($volumePpsLong)
  {
    $this->volumePpsLong = $volumePpsLong;
  }
  /**
   * @return string
   */
  public function getVolumePpsLong()
  {
    return $this->volumePpsLong;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudSecuritycenterV2Attack::class, 'Google_Service_SecurityCommandCenter_GoogleCloudSecuritycenterV2Attack');
