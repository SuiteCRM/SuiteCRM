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

namespace Google\Service\ContainerAnalysis;

class VulnerabilityAssessmentNote extends \Google\Model
{
  protected $assessmentType = Assessment::class;
  protected $assessmentDataType = '';
  /**
   * @var string
   */
  public $languageCode;
  /**
   * @var string
   */
  public $longDescription;
  protected $productType = Product::class;
  protected $productDataType = '';
  protected $publisherType = Publisher::class;
  protected $publisherDataType = '';
  /**
   * @var string
   */
  public $shortDescription;
  /**
   * @var string
   */
  public $title;

  /**
   * @param Assessment
   */
  public function setAssessment(Assessment $assessment)
  {
    $this->assessment = $assessment;
  }
  /**
   * @return Assessment
   */
  public function getAssessment()
  {
    return $this->assessment;
  }
  /**
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param string
   */
  public function setLongDescription($longDescription)
  {
    $this->longDescription = $longDescription;
  }
  /**
   * @return string
   */
  public function getLongDescription()
  {
    return $this->longDescription;
  }
  /**
   * @param Product
   */
  public function setProduct(Product $product)
  {
    $this->product = $product;
  }
  /**
   * @return Product
   */
  public function getProduct()
  {
    return $this->product;
  }
  /**
   * @param Publisher
   */
  public function setPublisher(Publisher $publisher)
  {
    $this->publisher = $publisher;
  }
  /**
   * @return Publisher
   */
  public function getPublisher()
  {
    return $this->publisher;
  }
  /**
   * @param string
   */
  public function setShortDescription($shortDescription)
  {
    $this->shortDescription = $shortDescription;
  }
  /**
   * @return string
   */
  public function getShortDescription()
  {
    return $this->shortDescription;
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
class_alias(VulnerabilityAssessmentNote::class, 'Google_Service_ContainerAnalysis_VulnerabilityAssessmentNote');
