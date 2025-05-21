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

class InputField extends \Google\Model
{
  protected $checkboxInputType = InputFieldCheckboxInput::class;
  protected $checkboxInputDataType = '';
  protected $choiceInputType = InputFieldChoiceInput::class;
  protected $choiceInputDataType = '';
  /**
   * @var string
   */
  public $id;
  protected $labelType = TextWithTooltip::class;
  protected $labelDataType = '';
  /**
   * @var bool
   */
  public $required;
  protected $textInputType = InputFieldTextInput::class;
  protected $textInputDataType = '';

  /**
   * @param InputFieldCheckboxInput
   */
  public function setCheckboxInput(InputFieldCheckboxInput $checkboxInput)
  {
    $this->checkboxInput = $checkboxInput;
  }
  /**
   * @return InputFieldCheckboxInput
   */
  public function getCheckboxInput()
  {
    return $this->checkboxInput;
  }
  /**
   * @param InputFieldChoiceInput
   */
  public function setChoiceInput(InputFieldChoiceInput $choiceInput)
  {
    $this->choiceInput = $choiceInput;
  }
  /**
   * @return InputFieldChoiceInput
   */
  public function getChoiceInput()
  {
    return $this->choiceInput;
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
   * @param TextWithTooltip
   */
  public function setLabel(TextWithTooltip $label)
  {
    $this->label = $label;
  }
  /**
   * @return TextWithTooltip
   */
  public function getLabel()
  {
    return $this->label;
  }
  /**
   * @param bool
   */
  public function setRequired($required)
  {
    $this->required = $required;
  }
  /**
   * @return bool
   */
  public function getRequired()
  {
    return $this->required;
  }
  /**
   * @param InputFieldTextInput
   */
  public function setTextInput(InputFieldTextInput $textInput)
  {
    $this->textInput = $textInput;
  }
  /**
   * @return InputFieldTextInput
   */
  public function getTextInput()
  {
    return $this->textInput;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(InputField::class, 'Google_Service_ShoppingContent_InputField');
