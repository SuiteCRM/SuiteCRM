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

class Recommendation extends \Google\Collection
{
  protected $collection_key = 'creative';
  protected $additionalCallToActionType = RecommendationCallToAction::class;
  protected $additionalCallToActionDataType = 'array';
  protected $additionalDescriptionsType = RecommendationDescription::class;
  protected $additionalDescriptionsDataType = 'array';
  protected $creativeType = RecommendationCreative::class;
  protected $creativeDataType = 'array';
  protected $defaultCallToActionType = RecommendationCallToAction::class;
  protected $defaultCallToActionDataType = '';
  /**
   * @var string
   */
  public $defaultDescription;
  /**
   * @var int
   */
  public $numericalImpact;
  /**
   * @var bool
   */
  public $paid;
  /**
   * @var string
   */
  public $recommendationName;
  /**
   * @var string
   */
  public $subType;
  /**
   * @var string
   */
  public $title;
  /**
   * @var string
   */
  public $type;

  /**
   * @param RecommendationCallToAction[]
   */
  public function setAdditionalCallToAction($additionalCallToAction)
  {
    $this->additionalCallToAction = $additionalCallToAction;
  }
  /**
   * @return RecommendationCallToAction[]
   */
  public function getAdditionalCallToAction()
  {
    return $this->additionalCallToAction;
  }
  /**
   * @param RecommendationDescription[]
   */
  public function setAdditionalDescriptions($additionalDescriptions)
  {
    $this->additionalDescriptions = $additionalDescriptions;
  }
  /**
   * @return RecommendationDescription[]
   */
  public function getAdditionalDescriptions()
  {
    return $this->additionalDescriptions;
  }
  /**
   * @param RecommendationCreative[]
   */
  public function setCreative($creative)
  {
    $this->creative = $creative;
  }
  /**
   * @return RecommendationCreative[]
   */
  public function getCreative()
  {
    return $this->creative;
  }
  /**
   * @param RecommendationCallToAction
   */
  public function setDefaultCallToAction(RecommendationCallToAction $defaultCallToAction)
  {
    $this->defaultCallToAction = $defaultCallToAction;
  }
  /**
   * @return RecommendationCallToAction
   */
  public function getDefaultCallToAction()
  {
    return $this->defaultCallToAction;
  }
  /**
   * @param string
   */
  public function setDefaultDescription($defaultDescription)
  {
    $this->defaultDescription = $defaultDescription;
  }
  /**
   * @return string
   */
  public function getDefaultDescription()
  {
    return $this->defaultDescription;
  }
  /**
   * @param int
   */
  public function setNumericalImpact($numericalImpact)
  {
    $this->numericalImpact = $numericalImpact;
  }
  /**
   * @return int
   */
  public function getNumericalImpact()
  {
    return $this->numericalImpact;
  }
  /**
   * @param bool
   */
  public function setPaid($paid)
  {
    $this->paid = $paid;
  }
  /**
   * @return bool
   */
  public function getPaid()
  {
    return $this->paid;
  }
  /**
   * @param string
   */
  public function setRecommendationName($recommendationName)
  {
    $this->recommendationName = $recommendationName;
  }
  /**
   * @return string
   */
  public function getRecommendationName()
  {
    return $this->recommendationName;
  }
  /**
   * @param string
   */
  public function setSubType($subType)
  {
    $this->subType = $subType;
  }
  /**
   * @return string
   */
  public function getSubType()
  {
    return $this->subType;
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
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Recommendation::class, 'Google_Service_ShoppingContent_Recommendation');
