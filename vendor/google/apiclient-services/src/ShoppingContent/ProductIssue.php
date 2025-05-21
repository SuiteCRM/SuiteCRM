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

class ProductIssue extends \Google\Collection
{
  protected $collection_key = 'actions';
  protected $actionsType = Action::class;
  protected $actionsDataType = 'array';
  protected $impactType = ProductIssueImpact::class;
  protected $impactDataType = '';
  /**
   * @var string
   */
  public $prerenderedContent;
  /**
   * @var string
   */
  public $prerenderedOutOfCourtDisputeSettlement;
  /**
   * @var string
   */
  public $title;

  /**
   * @param Action[]
   */
  public function setActions($actions)
  {
    $this->actions = $actions;
  }
  /**
   * @return Action[]
   */
  public function getActions()
  {
    return $this->actions;
  }
  /**
   * @param ProductIssueImpact
   */
  public function setImpact(ProductIssueImpact $impact)
  {
    $this->impact = $impact;
  }
  /**
   * @return ProductIssueImpact
   */
  public function getImpact()
  {
    return $this->impact;
  }
  /**
   * @param string
   */
  public function setPrerenderedContent($prerenderedContent)
  {
    $this->prerenderedContent = $prerenderedContent;
  }
  /**
   * @return string
   */
  public function getPrerenderedContent()
  {
    return $this->prerenderedContent;
  }
  /**
   * @param string
   */
  public function setPrerenderedOutOfCourtDisputeSettlement($prerenderedOutOfCourtDisputeSettlement)
  {
    $this->prerenderedOutOfCourtDisputeSettlement = $prerenderedOutOfCourtDisputeSettlement;
  }
  /**
   * @return string
   */
  public function getPrerenderedOutOfCourtDisputeSettlement()
  {
    return $this->prerenderedOutOfCourtDisputeSettlement;
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
class_alias(ProductIssue::class, 'Google_Service_ShoppingContent_ProductIssue');
