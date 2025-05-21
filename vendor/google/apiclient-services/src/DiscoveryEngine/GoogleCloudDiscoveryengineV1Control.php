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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1Control extends \Google\Collection
{
  protected $collection_key = 'useCases';
  /**
   * @var string[]
   */
  public $associatedServingConfigIds;
  protected $boostActionType = GoogleCloudDiscoveryengineV1ControlBoostAction::class;
  protected $boostActionDataType = '';
  protected $conditionsType = GoogleCloudDiscoveryengineV1Condition::class;
  protected $conditionsDataType = 'array';
  /**
   * @var string
   */
  public $displayName;
  protected $filterActionType = GoogleCloudDiscoveryengineV1ControlFilterAction::class;
  protected $filterActionDataType = '';
  /**
   * @var string
   */
  public $name;
  protected $promoteActionType = GoogleCloudDiscoveryengineV1ControlPromoteAction::class;
  protected $promoteActionDataType = '';
  protected $redirectActionType = GoogleCloudDiscoveryengineV1ControlRedirectAction::class;
  protected $redirectActionDataType = '';
  /**
   * @var string
   */
  public $solutionType;
  protected $synonymsActionType = GoogleCloudDiscoveryengineV1ControlSynonymsAction::class;
  protected $synonymsActionDataType = '';
  /**
   * @var string[]
   */
  public $useCases;

  /**
   * @param string[]
   */
  public function setAssociatedServingConfigIds($associatedServingConfigIds)
  {
    $this->associatedServingConfigIds = $associatedServingConfigIds;
  }
  /**
   * @return string[]
   */
  public function getAssociatedServingConfigIds()
  {
    return $this->associatedServingConfigIds;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1ControlBoostAction
   */
  public function setBoostAction(GoogleCloudDiscoveryengineV1ControlBoostAction $boostAction)
  {
    $this->boostAction = $boostAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ControlBoostAction
   */
  public function getBoostAction()
  {
    return $this->boostAction;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1Condition[]
   */
  public function setConditions($conditions)
  {
    $this->conditions = $conditions;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1Condition[]
   */
  public function getConditions()
  {
    return $this->conditions;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1ControlFilterAction
   */
  public function setFilterAction(GoogleCloudDiscoveryengineV1ControlFilterAction $filterAction)
  {
    $this->filterAction = $filterAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ControlFilterAction
   */
  public function getFilterAction()
  {
    return $this->filterAction;
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
   * @param GoogleCloudDiscoveryengineV1ControlPromoteAction
   */
  public function setPromoteAction(GoogleCloudDiscoveryengineV1ControlPromoteAction $promoteAction)
  {
    $this->promoteAction = $promoteAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ControlPromoteAction
   */
  public function getPromoteAction()
  {
    return $this->promoteAction;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1ControlRedirectAction
   */
  public function setRedirectAction(GoogleCloudDiscoveryengineV1ControlRedirectAction $redirectAction)
  {
    $this->redirectAction = $redirectAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ControlRedirectAction
   */
  public function getRedirectAction()
  {
    return $this->redirectAction;
  }
  /**
   * @param string
   */
  public function setSolutionType($solutionType)
  {
    $this->solutionType = $solutionType;
  }
  /**
   * @return string
   */
  public function getSolutionType()
  {
    return $this->solutionType;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1ControlSynonymsAction
   */
  public function setSynonymsAction(GoogleCloudDiscoveryengineV1ControlSynonymsAction $synonymsAction)
  {
    $this->synonymsAction = $synonymsAction;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1ControlSynonymsAction
   */
  public function getSynonymsAction()
  {
    return $this->synonymsAction;
  }
  /**
   * @param string[]
   */
  public function setUseCases($useCases)
  {
    $this->useCases = $useCases;
  }
  /**
   * @return string[]
   */
  public function getUseCases()
  {
    return $this->useCases;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1Control::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1Control');
