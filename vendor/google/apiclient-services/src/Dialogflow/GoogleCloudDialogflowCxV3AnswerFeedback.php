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

class GoogleCloudDialogflowCxV3AnswerFeedback extends \Google\Model
{
  /**
   * @var string
   */
  public $customRating;
  /**
   * @var string
   */
  public $rating;
  protected $ratingReasonType = GoogleCloudDialogflowCxV3AnswerFeedbackRatingReason::class;
  protected $ratingReasonDataType = '';

  /**
   * @param string
   */
  public function setCustomRating($customRating)
  {
    $this->customRating = $customRating;
  }
  /**
   * @return string
   */
  public function getCustomRating()
  {
    return $this->customRating;
  }
  /**
   * @param string
   */
  public function setRating($rating)
  {
    $this->rating = $rating;
  }
  /**
   * @return string
   */
  public function getRating()
  {
    return $this->rating;
  }
  /**
   * @param GoogleCloudDialogflowCxV3AnswerFeedbackRatingReason
   */
  public function setRatingReason(GoogleCloudDialogflowCxV3AnswerFeedbackRatingReason $ratingReason)
  {
    $this->ratingReason = $ratingReason;
  }
  /**
   * @return GoogleCloudDialogflowCxV3AnswerFeedbackRatingReason
   */
  public function getRatingReason()
  {
    return $this->ratingReason;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3AnswerFeedback::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3AnswerFeedback');
