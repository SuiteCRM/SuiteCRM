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

class ReportInteractionRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $interactionType;
  /**
   * @var string
   */
  public $responseToken;
  /**
   * @var string
   */
  public $subtype;
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setInteractionType($interactionType)
  {
    $this->interactionType = $interactionType;
  }
  /**
   * @return string
   */
  public function getInteractionType()
  {
    return $this->interactionType;
  }
  /**
   * @param string
   */
  public function setResponseToken($responseToken)
  {
    $this->responseToken = $responseToken;
  }
  /**
   * @return string
   */
  public function getResponseToken()
  {
    return $this->responseToken;
  }
  /**
   * @param string
   */
  public function setSubtype($subtype)
  {
    $this->subtype = $subtype;
  }
  /**
   * @return string
   */
  public function getSubtype()
  {
    return $this->subtype;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ReportInteractionRequest::class, 'Google_Service_ShoppingContent_ReportInteractionRequest');
