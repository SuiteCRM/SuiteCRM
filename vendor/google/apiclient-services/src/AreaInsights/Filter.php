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

namespace Google\Service\AreaInsights;

class Filter extends \Google\Collection
{
  protected $collection_key = 'priceLevels';
  protected $locationFilterType = LocationFilter::class;
  protected $locationFilterDataType = '';
  /**
   * @var string[]
   */
  public $operatingStatus;
  /**
   * @var string[]
   */
  public $priceLevels;
  protected $ratingFilterType = RatingFilter::class;
  protected $ratingFilterDataType = '';
  protected $typeFilterType = TypeFilter::class;
  protected $typeFilterDataType = '';

  /**
   * @param LocationFilter
   */
  public function setLocationFilter(LocationFilter $locationFilter)
  {
    $this->locationFilter = $locationFilter;
  }
  /**
   * @return LocationFilter
   */
  public function getLocationFilter()
  {
    return $this->locationFilter;
  }
  /**
   * @param string[]
   */
  public function setOperatingStatus($operatingStatus)
  {
    $this->operatingStatus = $operatingStatus;
  }
  /**
   * @return string[]
   */
  public function getOperatingStatus()
  {
    return $this->operatingStatus;
  }
  /**
   * @param string[]
   */
  public function setPriceLevels($priceLevels)
  {
    $this->priceLevels = $priceLevels;
  }
  /**
   * @return string[]
   */
  public function getPriceLevels()
  {
    return $this->priceLevels;
  }
  /**
   * @param RatingFilter
   */
  public function setRatingFilter(RatingFilter $ratingFilter)
  {
    $this->ratingFilter = $ratingFilter;
  }
  /**
   * @return RatingFilter
   */
  public function getRatingFilter()
  {
    return $this->ratingFilter;
  }
  /**
   * @param TypeFilter
   */
  public function setTypeFilter(TypeFilter $typeFilter)
  {
    $this->typeFilter = $typeFilter;
  }
  /**
   * @return TypeFilter
   */
  public function getTypeFilter()
  {
    return $this->typeFilter;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Filter::class, 'Google_Service_AreaInsights_Filter');
