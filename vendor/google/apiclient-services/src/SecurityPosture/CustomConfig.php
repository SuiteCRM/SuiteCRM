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

namespace Google\Service\SecurityPosture;

class CustomConfig extends \Google\Model
{
  protected $customOutputType = CustomOutputSpec::class;
  protected $customOutputDataType = '';
  /**
   * @var string
   */
  public $description;
  protected $predicateType = Expr::class;
  protected $predicateDataType = '';
  /**
   * @var string
   */
  public $recommendation;
  protected $resourceSelectorType = ResourceSelector::class;
  protected $resourceSelectorDataType = '';
  /**
   * @var string
   */
  public $severity;

  /**
   * @param CustomOutputSpec
   */
  public function setCustomOutput(CustomOutputSpec $customOutput)
  {
    $this->customOutput = $customOutput;
  }
  /**
   * @return CustomOutputSpec
   */
  public function getCustomOutput()
  {
    return $this->customOutput;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param Expr
   */
  public function setPredicate(Expr $predicate)
  {
    $this->predicate = $predicate;
  }
  /**
   * @return Expr
   */
  public function getPredicate()
  {
    return $this->predicate;
  }
  /**
   * @param string
   */
  public function setRecommendation($recommendation)
  {
    $this->recommendation = $recommendation;
  }
  /**
   * @return string
   */
  public function getRecommendation()
  {
    return $this->recommendation;
  }
  /**
   * @param ResourceSelector
   */
  public function setResourceSelector(ResourceSelector $resourceSelector)
  {
    $this->resourceSelector = $resourceSelector;
  }
  /**
   * @return ResourceSelector
   */
  public function getResourceSelector()
  {
    return $this->resourceSelector;
  }
  /**
   * @param string
   */
  public function setSeverity($severity)
  {
    $this->severity = $severity;
  }
  /**
   * @return string
   */
  public function getSeverity()
  {
    return $this->severity;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CustomConfig::class, 'Google_Service_SecurityPosture_CustomConfig');
