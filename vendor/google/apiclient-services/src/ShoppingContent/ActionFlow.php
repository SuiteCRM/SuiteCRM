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

class ActionFlow extends \Google\Collection
{
  protected $collection_key = 'inputs';
  /**
   * @var string
   */
  public $dialogButtonLabel;
  protected $dialogCalloutType = Callout::class;
  protected $dialogCalloutDataType = '';
  protected $dialogMessageType = TextWithTooltip::class;
  protected $dialogMessageDataType = '';
  /**
   * @var string
   */
  public $dialogTitle;
  /**
   * @var string
   */
  public $id;
  protected $inputsType = InputField::class;
  protected $inputsDataType = 'array';
  /**
   * @var string
   */
  public $label;

  /**
   * @param string
   */
  public function setDialogButtonLabel($dialogButtonLabel)
  {
    $this->dialogButtonLabel = $dialogButtonLabel;
  }
  /**
   * @return string
   */
  public function getDialogButtonLabel()
  {
    return $this->dialogButtonLabel;
  }
  /**
   * @param Callout
   */
  public function setDialogCallout(Callout $dialogCallout)
  {
    $this->dialogCallout = $dialogCallout;
  }
  /**
   * @return Callout
   */
  public function getDialogCallout()
  {
    return $this->dialogCallout;
  }
  /**
   * @param TextWithTooltip
   */
  public function setDialogMessage(TextWithTooltip $dialogMessage)
  {
    $this->dialogMessage = $dialogMessage;
  }
  /**
   * @return TextWithTooltip
   */
  public function getDialogMessage()
  {
    return $this->dialogMessage;
  }
  /**
   * @param string
   */
  public function setDialogTitle($dialogTitle)
  {
    $this->dialogTitle = $dialogTitle;
  }
  /**
   * @return string
   */
  public function getDialogTitle()
  {
    return $this->dialogTitle;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param InputField[]
   */
  public function setInputs($inputs)
  {
    $this->inputs = $inputs;
  }
  /**
   * @return InputField[]
   */
  public function getInputs()
  {
    return $this->inputs;
  }
  /**
   * @param string
   */
  public function setLabel($label)
  {
    $this->label = $label;
  }
  /**
   * @return string
   */
  public function getLabel()
  {
    return $this->label;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ActionFlow::class, 'Google_Service_ShoppingContent_ActionFlow');
