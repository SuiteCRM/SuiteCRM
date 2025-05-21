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

namespace Google\Service\ShoppingContent;

class CompetitiveVisibility extends \Google\Model
{
  public $adsOrganicRatio;
  public $categoryBenchmarkVisibilityTrend;
  /**
   * @var string
   */
  public $categoryId;
  /**
   * @var string
   */
  public $countryCode;
  protected $dateType = Date::class;
  protected $dateDataType = '';
  /**
   * @var string
   */
  public $domain;
  public $higherPositionRate;
  /**
   * @var bool
   */
  public $isYourDomain;
  public $pageOverlapRate;
  /**
   * @var string
   */
  public $rank;
  public $relativeVisibility;
  /**
   * @var string
   */
  public $trafficSource;
  public $yourDomainVisibilityTrend;

  public function setAdsOrganicRatio($adsOrganicRatio)
  {
    $this->adsOrganicRatio = $adsOrganicRatio;
  }
  public function getAdsOrganicRatio()
  {
    return $this->adsOrganicRatio;
  }
  public function setCategoryBenchmarkVisibilityTrend($categoryBenchmarkVisibilityTrend)
  {
    $this->categoryBenchmarkVisibilityTrend = $categoryBenchmarkVisibilityTrend;
  }
  public function getCategoryBenchmarkVisibilityTrend()
  {
    return $this->categoryBenchmarkVisibilityTrend;
  }
  /**
   * @param string
   */
  public function setCategoryId($categoryId)
  {
    $this->categoryId = $categoryId;
  }
  /**
   * @return string
   */
  public function getCategoryId()
  {
    return $this->categoryId;
  }
  /**
   * @param string
   */
  public function setCountryCode($countryCode)
  {
    $this->countryCode = $countryCode;
  }
  /**
   * @return string
   */
  public function getCountryCode()
  {
    return $this->countryCode;
  }
  /**
   * @param Date
   */
  public function setDate(Date $date)
  {
    $this->date = $date;
  }
  /**
   * @return Date
   */
  public function getDate()
  {
    return $this->date;
  }
  /**
   * @param string
   */
  public function setDomain($domain)
  {
    $this->domain = $domain;
  }
  /**
   * @return string
   */
  public function getDomain()
  {
    return $this->domain;
  }
  public function setHigherPositionRate($higherPositionRate)
  {
    $this->higherPositionRate = $higherPositionRate;
  }
  public function getHigherPositionRate()
  {
    return $this->higherPositionRate;
  }
  /**
   * @param bool
   */
  public function setIsYourDomain($isYourDomain)
  {
    $this->isYourDomain = $isYourDomain;
  }
  /**
   * @return bool
   */
  public function getIsYourDomain()
  {
    return $this->isYourDomain;
  }
  public function setPageOverlapRate($pageOverlapRate)
  {
    $this->pageOverlapRate = $pageOverlapRate;
  }
  public function getPageOverlapRate()
  {
    return $this->pageOverlapRate;
  }
  /**
   * @param string
   */
  public function setRank($rank)
  {
    $this->rank = $rank;
  }
  /**
   * @return string
   */
  public function getRank()
  {
    return $this->rank;
  }
  public function setRelativeVisibility($relativeVisibility)
  {
    $this->relativeVisibility = $relativeVisibility;
  }
  public function getRelativeVisibility()
  {
    return $this->relativeVisibility;
  }
  /**
   * @param string
   */
  public function setTrafficSource($trafficSource)
  {
    $this->trafficSource = $trafficSource;
  }
  /**
   * @return string
   */
  public function getTrafficSource()
  {
    return $this->trafficSource;
  }
  public function setYourDomainVisibilityTrend($yourDomainVisibilityTrend)
  {
    $this->yourDomainVisibilityTrend = $yourDomainVisibilityTrend;
  }
  public function getYourDomainVisibilityTrend()
  {
    return $this->yourDomainVisibilityTrend;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CompetitiveVisibility::class, 'Google_Service_ShoppingContent_CompetitiveVisibility');
