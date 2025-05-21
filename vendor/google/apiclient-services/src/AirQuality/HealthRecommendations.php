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

class HealthRecommendations extends \Google\Model
{
  /**
   * @var string
   */
  public $athletes;
  /**
   * @var string
   */
  public $children;
  /**
   * @var string
   */
  public $elderly;
  /**
   * @var string
   */
  public $generalPopulation;
  /**
   * @var string
   */
  public $heartDiseasePopulation;
  /**
   * @var string
   */
  public $lungDiseasePopulation;
  /**
   * @var string
   */
  public $pregnantWomen;

  /**
   * @param string
   */
  public function setAthletes($athletes)
  {
    $this->athletes = $athletes;
  }
  /**
   * @return string
   */
  public function getAthletes()
  {
    return $this->athletes;
  }
  /**
   * @param string
   */
  public function setChildren($children)
  {
    $this->children = $children;
  }
  /**
   * @return string
   */
  public function getChildren()
  {
    return $this->children;
  }
  /**
   * @param string
   */
  public function setElderly($elderly)
  {
    $this->elderly = $elderly;
  }
  /**
   * @return string
   */
  public function getElderly()
  {
    return $this->elderly;
  }
  /**
   * @param string
   */
  public function setGeneralPopulation($generalPopulation)
  {
    $this->generalPopulation = $generalPopulation;
  }
  /**
   * @return string
   */
  public function getGeneralPopulation()
  {
    return $this->generalPopulation;
  }
  /**
   * @param string
   */
  public function setHeartDiseasePopulation($heartDiseasePopulation)
  {
    $this->heartDiseasePopulation = $heartDiseasePopulation;
  }
  /**
   * @return string
   */
  public function getHeartDiseasePopulation()
  {
    return $this->heartDiseasePopulation;
  }
  /**
   * @param string
   */
  public function setLungDiseasePopulation($lungDiseasePopulation)
  {
    $this->lungDiseasePopulation = $lungDiseasePopulation;
  }
  /**
   * @return string
   */
  public function getLungDiseasePopulation()
  {
    return $this->lungDiseasePopulation;
  }
  /**
   * @param string
   */
  public function setPregnantWomen($pregnantWomen)
  {
    $this->pregnantWomen = $pregnantWomen;
  }
  /**
   * @return string
   */
  public function getPregnantWomen()
  {
    return $this->pregnantWomen;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HealthRecommendations::class, 'Google_Service_AirQuality_HealthRecommendations');
