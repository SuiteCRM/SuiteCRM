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

namespace Google\Service\TravelImpactModel;

class EmissionsGramsPerPax extends \Google\Model
{
  /**
   * @var int
   */
  public $business;
  /**
   * @var int
   */
  public $economy;
  /**
   * @var int
   */
  public $first;
  /**
   * @var int
   */
  public $premiumEconomy;

  /**
   * @param int
   */
  public function setBusiness($business)
  {
    $this->business = $business;
  }
  /**
   * @return int
   */
  public function getBusiness()
  {
    return $this->business;
  }
  /**
   * @param int
   */
  public function setEconomy($economy)
  {
    $this->economy = $economy;
  }
  /**
   * @return int
   */
  public function getEconomy()
  {
    return $this->economy;
  }
  /**
   * @param int
   */
  public function setFirst($first)
  {
    $this->first = $first;
  }
  /**
   * @return int
   */
  public function getFirst()
  {
    return $this->first;
  }
  /**
   * @param int
   */
  public function setPremiumEconomy($premiumEconomy)
  {
    $this->premiumEconomy = $premiumEconomy;
  }
  /**
   * @return int
   */
  public function getPremiumEconomy()
  {
    return $this->premiumEconomy;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EmissionsGramsPerPax::class, 'Google_Service_TravelImpactModel_EmissionsGramsPerPax');
