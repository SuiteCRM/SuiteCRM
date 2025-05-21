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

class GoogleAdsSearchads360V0CommonLocationGroupInfo extends \Google\Collection
{
  protected $collection_key = 'geoTargetConstants';
  /**
   * @var string[]
   */
  public $feedItemSets;
  /**
   * @var string[]
   */
  public $geoTargetConstants;
  /**
   * @var string
   */
  public $radius;
  /**
   * @var string
   */
  public $radiusUnits;

  /**
   * @param string[]
   */
  public function setFeedItemSets($feedItemSets)
  {
    $this->feedItemSets = $feedItemSets;
  }
  /**
   * @return string[]
   */
  public function getFeedItemSets()
  {
    return $this->feedItemSets;
  }
  /**
   * @param string[]
   */
  public function setGeoTargetConstants($geoTargetConstants)
  {
    $this->geoTargetConstants = $geoTargetConstants;
  }
  /**
   * @return string[]
   */
  public function getGeoTargetConstants()
  {
    return $this->geoTargetConstants;
  }
  /**
   * @param string
   */
  public function setRadius($radius)
  {
    $this->radius = $radius;
  }
  /**
   * @return string
   */
  public function getRadius()
  {
    return $this->radius;
  }
  /**
   * @param string
   */
  public function setRadiusUnits($radiusUnits)
  {
    $this->radiusUnits = $radiusUnits;
  }
  /**
   * @return string
   */
  public function getRadiusUnits()
  {
    return $this->radiusUnits;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0CommonLocationGroupInfo::class, 'Google_Service_SA360_GoogleAdsSearchads360V0CommonLocationGroupInfo');
