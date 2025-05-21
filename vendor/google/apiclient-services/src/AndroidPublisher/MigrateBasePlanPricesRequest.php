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

class MigrateBasePlanPricesRequest extends \Google\Collection
{
  protected $collection_key = 'regionalPriceMigrations';
  /**
   * @var string
   */
  public $basePlanId;
  /**
   * @var string
   */
  public $latencyTolerance;
  /**
   * @var string
   */
  public $packageName;
  /**
   * @var string
   */
  public $productId;
  protected $regionalPriceMigrationsType = RegionalPriceMigrationConfig::class;
  protected $regionalPriceMigrationsDataType = 'array';
  protected $regionsVersionType = RegionsVersion::class;
  protected $regionsVersionDataType = '';

  /**
   * @param string
   */
  public function setBasePlanId($basePlanId)
  {
    $this->basePlanId = $basePlanId;
  }
  /**
   * @return string
   */
  public function getBasePlanId()
  {
    return $this->basePlanId;
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
   * @param string
   */
  public function setPackageName($packageName)
  {
    $this->packageName = $packageName;
  }
  /**
   * @return string
   */
  public function getPackageName()
  {
    return $this->packageName;
  }
  /**
   * @param string
   */
  public function setProductId($productId)
  {
    $this->productId = $productId;
  }
  /**
   * @return string
   */
  public function getProductId()
  {
    return $this->productId;
  }
  /**
   * @param RegionalPriceMigrationConfig[]
   */
  public function setRegionalPriceMigrations($regionalPriceMigrations)
  {
    $this->regionalPriceMigrations = $regionalPriceMigrations;
  }
  /**
   * @return RegionalPriceMigrationConfig[]
   */
  public function getRegionalPriceMigrations()
  {
    return $this->regionalPriceMigrations;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(MigrateBasePlanPricesRequest::class, 'Google_Service_AndroidPublisher_MigrateBasePlanPricesRequest');
