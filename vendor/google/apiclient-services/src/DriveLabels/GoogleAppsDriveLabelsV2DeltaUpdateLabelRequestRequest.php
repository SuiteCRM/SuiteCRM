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

namespace Google\Service\DriveLabels;

class GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestRequest extends \Google\Model
{
  protected $createFieldType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestCreateFieldRequest::class;
  protected $createFieldDataType = '';
  protected $createSelectionChoiceType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestCreateSelectionChoiceRequest::class;
  protected $createSelectionChoiceDataType = '';
  protected $deleteFieldType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDeleteFieldRequest::class;
  protected $deleteFieldDataType = '';
  protected $deleteSelectionChoiceType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDeleteSelectionChoiceRequest::class;
  protected $deleteSelectionChoiceDataType = '';
  protected $disableFieldType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDisableFieldRequest::class;
  protected $disableFieldDataType = '';
  protected $disableSelectionChoiceType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDisableSelectionChoiceRequest::class;
  protected $disableSelectionChoiceDataType = '';
  protected $enableFieldType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestEnableFieldRequest::class;
  protected $enableFieldDataType = '';
  protected $enableSelectionChoiceType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestEnableSelectionChoiceRequest::class;
  protected $enableSelectionChoiceDataType = '';
  protected $updateFieldDataType = '';
  protected $updateFieldTypeType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateFieldTypeRequest::class;
  protected $updateFieldTypeDataType = '';
  protected $updateLabelType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateLabelPropertiesRequest::class;
  protected $updateLabelDataType = '';
  protected $updateSelectionChoicePropertiesType = GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateSelectionChoicePropertiesRequest::class;
  protected $updateSelectionChoicePropertiesDataType = '';

  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestCreateFieldRequest
   */
  public function setCreateField(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestCreateFieldRequest $createField)
  {
    $this->createField = $createField;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestCreateFieldRequest
   */
  public function getCreateField()
  {
    return $this->createField;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestCreateSelectionChoiceRequest
   */
  public function setCreateSelectionChoice(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestCreateSelectionChoiceRequest $createSelectionChoice)
  {
    $this->createSelectionChoice = $createSelectionChoice;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestCreateSelectionChoiceRequest
   */
  public function getCreateSelectionChoice()
  {
    return $this->createSelectionChoice;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDeleteFieldRequest
   */
  public function setDeleteField(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDeleteFieldRequest $deleteField)
  {
    $this->deleteField = $deleteField;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDeleteFieldRequest
   */
  public function getDeleteField()
  {
    return $this->deleteField;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDeleteSelectionChoiceRequest
   */
  public function setDeleteSelectionChoice(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDeleteSelectionChoiceRequest $deleteSelectionChoice)
  {
    $this->deleteSelectionChoice = $deleteSelectionChoice;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDeleteSelectionChoiceRequest
   */
  public function getDeleteSelectionChoice()
  {
    return $this->deleteSelectionChoice;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDisableFieldRequest
   */
  public function setDisableField(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDisableFieldRequest $disableField)
  {
    $this->disableField = $disableField;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDisableFieldRequest
   */
  public function getDisableField()
  {
    return $this->disableField;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDisableSelectionChoiceRequest
   */
  public function setDisableSelectionChoice(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDisableSelectionChoiceRequest $disableSelectionChoice)
  {
    $this->disableSelectionChoice = $disableSelectionChoice;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestDisableSelectionChoiceRequest
   */
  public function getDisableSelectionChoice()
  {
    return $this->disableSelectionChoice;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestEnableFieldRequest
   */
  public function setEnableField(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestEnableFieldRequest $enableField)
  {
    $this->enableField = $enableField;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestEnableFieldRequest
   */
  public function getEnableField()
  {
    return $this->enableField;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestEnableSelectionChoiceRequest
   */
  public function setEnableSelectionChoice(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestEnableSelectionChoiceRequest $enableSelectionChoice)
  {
    $this->enableSelectionChoice = $enableSelectionChoice;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestEnableSelectionChoiceRequest
   */
  public function getEnableSelectionChoice()
  {
    return $this->enableSelectionChoice;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateFieldPropertiesRequest
   */
  public function setUpdateField(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateFieldPropertiesRequest $updateField)
  {
    $this->updateField = $updateField;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateFieldPropertiesRequest
   */
  public function getUpdateField()
  {
    return $this->updateField;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateFieldTypeRequest
   */
  public function setUpdateFieldType(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateFieldTypeRequest $updateFieldType)
  {
    $this->updateFieldType = $updateFieldType;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateFieldTypeRequest
   */
  public function getUpdateFieldType()
  {
    return $this->updateFieldType;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateLabelPropertiesRequest
   */
  public function setUpdateLabel(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateLabelPropertiesRequest $updateLabel)
  {
    $this->updateLabel = $updateLabel;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateLabelPropertiesRequest
   */
  public function getUpdateLabel()
  {
    return $this->updateLabel;
  }
  /**
   * @param GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateSelectionChoicePropertiesRequest
   */
  public function setUpdateSelectionChoiceProperties(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateSelectionChoicePropertiesRequest $updateSelectionChoiceProperties)
  {
    $this->updateSelectionChoiceProperties = $updateSelectionChoiceProperties;
  }
  /**
   * @return GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestUpdateSelectionChoicePropertiesRequest
   */
  public function getUpdateSelectionChoiceProperties()
  {
    return $this->updateSelectionChoiceProperties;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestRequest::class, 'Google_Service_DriveLabels_GoogleAppsDriveLabelsV2DeltaUpdateLabelRequestRequest');
