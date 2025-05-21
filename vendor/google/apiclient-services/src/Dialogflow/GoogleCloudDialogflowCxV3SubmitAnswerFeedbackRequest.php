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

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowCxV3SubmitAnswerFeedbackRequest extends \Google\Model
{
  protected $answerFeedbackType = GoogleCloudDialogflowCxV3AnswerFeedback::class;
  protected $answerFeedbackDataType = '';
  /**
   * @var string
   */
  public $responseId;
  /**
   * @var string
   */
  public $updateMask;

  /**
   * @param GoogleCloudDialogflowCxV3AnswerFeedback
   */
  public function setAnswerFeedback(GoogleCloudDialogflowCxV3AnswerFeedback $answerFeedback)
  {
    $this->answerFeedback = $answerFeedback;
  }
  /**
   * @return GoogleCloudDialogflowCxV3AnswerFeedback
   */
  public function getAnswerFeedback()
  {
    return $this->answerFeedback;
  }
  /**
   * @param string
   */
  public function setResponseId($responseId)
  {
    $this->responseId = $responseId;
  }
  /**
   * @return string
   */
  public function getResponseId()
  {
    return $this->responseId;
  }
  /**
   * @param string
   */
  public function setUpdateMask($updateMask)
  {
    $this->updateMask = $updateMask;
  }
  /**
   * @return string
   */
  public function getUpdateMask()
  {
    return $this->updateMask;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3SubmitAnswerFeedbackRequest::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3SubmitAnswerFeedbackRequest');
