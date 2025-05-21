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

namespace Google\Service\ManufacturerCenter;

class ProductCertification extends \Google\Collection
{
  protected $collection_key = 'productType';
  /**
   * @var string
   */
  public $brand;
  protected $certificationType = Certification::class;
  protected $certificationDataType = 'array';
  /**
   * @var string[]
   */
  public $countryCode;
  protected $destinationStatusesType = DestinationStatus::class;
  protected $destinationStatusesDataType = 'array';
  protected $issuesType = Issue::class;
  protected $issuesDataType = 'array';
  /**
   * @var string[]
   */
  public $mpn;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string[]
   */
  public $productCode;
  /**
   * @var string[]
   */
  public $productType;
  /**
   * @var string
   */
  public $title;

  /**
   * @param string
   */
  public function setBrand($brand)
  {
    $this->brand = $brand;
  }
  /**
   * @return string
   */
  public function getBrand()
  {
    return $this->brand;
  }
  /**
   * @param Certification[]
   */
  public function setCertification($certification)
  {
    $this->certification = $certification;
  }
  /**
   * @return Certification[]
   */
  public function getCertification()
  {
    return $this->certification;
  }
  /**
   * @param string[]
   */
  public function setCountryCode($countryCode)
  {
    $this->countryCode = $countryCode;
  }
  /**
   * @return string[]
   */
  public function getCountryCode()
  {
    return $this->countryCode;
  }
  /**
   * @param DestinationStatus[]
   */
  public function setDestinationStatuses($destinationStatuses)
  {
    $this->destinationStatuses = $destinationStatuses;
  }
  /**
   * @return DestinationStatus[]
   */
  public function getDestinationStatuses()
  {
    return $this->destinationStatuses;
  }
  /**
   * @param Issue[]
   */
  public function setIssues($issues)
  {
    $this->issues = $issues;
  }
  /**
   * @return Issue[]
   */
  public function getIssues()
  {
    return $this->issues;
  }
  /**
   * @param string[]
   */
  public function setMpn($mpn)
  {
    $this->mpn = $mpn;
  }
  /**
   * @return string[]
   */
  public function getMpn()
  {
    return $this->mpn;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string[]
   */
  public function setProductCode($productCode)
  {
    $this->productCode = $productCode;
  }
  /**
   * @return string[]
   */
  public function getProductCode()
  {
    return $this->productCode;
  }
  /**
   * @param string[]
   */
  public function setProductType($productType)
  {
    $this->productType = $productType;
  }
  /**
   * @return string[]
   */
  public function getProductType()
  {
    return $this->productType;
  }
  /**
   * @param string
   */
  public function setTitle($title)
  {
    $this->title = $title;
  }
  /**
   * @return string
   */
  public function getTitle()
  {
    return $this->title;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProductCertification::class, 'Google_Service_ManufacturerCenter_ProductCertification');
