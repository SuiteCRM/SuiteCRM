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

class Action extends \Google\Collection
{
  protected $collection_key = 'reasons';
  protected $builtinSimpleActionType = BuiltInSimpleAction::class;
  protected $builtinSimpleActionDataType = '';
  protected $builtinUserInputActionType = BuiltInUserInputAction::class;
  protected $builtinUserInputActionDataType = '';
  /**
   * @var string
   */
  public $buttonLabel;
  protected $externalActionType = ExternalAction::class;
  protected $externalActionDataType = '';
  /**
   * @var bool
   */
  public $isAvailable;
  protected $reasonsType = ActionReason::class;
  protected $reasonsDataType = 'array';

  /**
   * @param BuiltInSimpleAction
   */
  public function setBuiltinSimpleAction(BuiltInSimpleAction $builtinSimpleAction)
  {
    $this->builtinSimpleAction = $builtinSimpleAction;
  }
  /**
   * @return BuiltInSimpleAction
   */
  public function getBuiltinSimpleAction()
  {
    return $this->builtinSimpleAction;
  }
  /**
   * @param BuiltInUserInputAction
   */
  public function setBuiltinUserInputAction(BuiltInUserInputAction $builtinUserInputAction)
  {
    $this->builtinUserInputAction = $builtinUserInputAction;
  }
  /**
   * @return BuiltInUserInputAction
   */
  public function getBuiltinUserInputAction()
  {
    return $this->builtinUserInputAction;
  }
  /**
   * @param string
   */
  public function setButtonLabel($buttonLabel)
  {
    $this->buttonLabel = $buttonLabel;
  }
  /**
   * @return string
   */
  public function getButtonLabel()
  {
    return $this->buttonLabel;
  }
  /**
   * @param ExternalAction
   */
  public function setExternalAction(ExternalAction $externalAction)
  {
    $this->externalAction = $externalAction;
  }
  /**
   * @return ExternalAction
   */
  public function getExternalAction()
  {
    return $this->externalAction;
  }
  /**
   * @param bool
   */
  public function setIsAvailable($isAvailable)
  {
    $this->isAvailable = $isAvailable;
  }
  /**
   * @return bool
   */
  public function getIsAvailable()
  {
    return $this->isAvailable;
  }
  /**
   * @param ActionReason[]
   */
  public function setReasons($reasons)
  {
    $this->reasons = $reasons;
  }
  /**
   * @return ActionReason[]
   */
  public function getReasons()
  {
    return $this->reasons;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Action::class, 'Google_Service_ShoppingContent_Action');
