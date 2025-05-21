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

namespace Google\Service\Solar;

class DataLayers extends \Google\Collection
{
  protected $collection_key = 'hourlyShadeUrls';
  /**
   * @var string
   */
  public $annualFluxUrl;
  /**
   * @var string
   */
  public $dsmUrl;
  /**
   * @var string[]
   */
  public $hourlyShadeUrls;
  protected $imageryDateType = Date::class;
  protected $imageryDateDataType = '';
  protected $imageryProcessedDateType = Date::class;
  protected $imageryProcessedDateDataType = '';
  /**
   * @var string
   */
  public $imageryQuality;
  /**
   * @var string
   */
  public $maskUrl;
  /**
   * @var string
   */
  public $monthlyFluxUrl;
  /**
   * @var string
   */
  public $rgbUrl;

  /**
   * @param string
   */
  public function setAnnualFluxUrl($annualFluxUrl)
  {
    $this->annualFluxUrl = $annualFluxUrl;
  }
  /**
   * @return string
   */
  public function getAnnualFluxUrl()
  {
    return $this->annualFluxUrl;
  }
  /**
   * @param string
   */
  public function setDsmUrl($dsmUrl)
  {
    $this->dsmUrl = $dsmUrl;
  }
  /**
   * @return string
   */
  public function getDsmUrl()
  {
    return $this->dsmUrl;
  }
  /**
   * @param string[]
   */
  public function setHourlyShadeUrls($hourlyShadeUrls)
  {
    $this->hourlyShadeUrls = $hourlyShadeUrls;
  }
  /**
   * @return string[]
   */
  public function getHourlyShadeUrls()
  {
    return $this->hourlyShadeUrls;
  }
  /**
   * @param Date
   */
  public function setImageryDate(Date $imageryDate)
  {
    $this->imageryDate = $imageryDate;
  }
  /**
   * @return Date
   */
  public function getImageryDate()
  {
    return $this->imageryDate;
  }
  /**
   * @param Date
   */
  public function setImageryProcessedDate(Date $imageryProcessedDate)
  {
    $this->imageryProcessedDate = $imageryProcessedDate;
  }
  /**
   * @return Date
   */
  public function getImageryProcessedDate()
  {
    return $this->imageryProcessedDate;
  }
  /**
   * @param string
   */
  public function setImageryQuality($imageryQuality)
  {
    $this->imageryQuality = $imageryQuality;
  }
  /**
   * @return string
   */
  public function getImageryQuality()
  {
    return $this->imageryQuality;
  }
  /**
   * @param string
   */
  public function setMaskUrl($maskUrl)
  {
    $this->maskUrl = $maskUrl;
  }
  /**
   * @return string
   */
  public function getMaskUrl()
  {
    return $this->maskUrl;
  }
  /**
   * @param string
   */
  public function setMonthlyFluxUrl($monthlyFluxUrl)
  {
    $this->monthlyFluxUrl = $monthlyFluxUrl;
  }
  /**
   * @return string
   */
  public function getMonthlyFluxUrl()
  {
    return $this->monthlyFluxUrl;
  }
  /**
   * @param string
   */
  public function setRgbUrl($rgbUrl)
  {
    $this->rgbUrl = $rgbUrl;
  }
  /**
   * @return string
   */
  public function getRgbUrl()
  {
    return $this->rgbUrl;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(DataLayers::class, 'Google_Service_Solar_DataLayers');
