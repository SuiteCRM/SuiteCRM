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

class TriggerActionPayload extends \Google\Model
{
  /**
   * @var string
   */
  public $actionContext;
  protected $actionInputType = ActionInput::class;
  protected $actionInputDataType = '';

  /**
   * @param string
   */
  public function setActionContext($actionContext)
  {
    $this->actionContext = $actionContext;
  }
  /**
   * @return string
   */
  public function getActionContext()
  {
    return $this->actionContext;
  }
  /**
   * @param ActionInput
   */
  public function setActionInput(ActionInput $actionInput)
  {
    $this->actionInput = $actionInput;
  }
  /**
   * @return ActionInput
   */
  public function getActionInput()
  {
    return $this->actionInput;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TriggerActionPayload::class, 'Google_Service_ShoppingContent_TriggerActionPayload');
