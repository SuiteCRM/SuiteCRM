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

namespace Google\Service\CloudSearch;

class SpellResult extends \Google\Model
{
  /**
   * @var string
   */
  public $suggestedQuery;
  protected $suggestedQueryHtmlType = SafeHtmlProto::class;
  protected $suggestedQueryHtmlDataType = '';
  /**
   * @var string
   */
  public $suggestionType;

  /**
   * @param string
   */
  public function setSuggestedQuery($suggestedQuery)
  {
    $this->suggestedQuery = $suggestedQuery;
  }
  /**
   * @return string
   */
  public function getSuggestedQuery()
  {
    return $this->suggestedQuery;
  }
  /**
   * @param SafeHtmlProto
   */
  public function setSuggestedQueryHtml(SafeHtmlProto $suggestedQueryHtml)
  {
    $this->suggestedQueryHtml = $suggestedQueryHtml;
  }
  /**
   * @return SafeHtmlProto
   */
  public function getSuggestedQueryHtml()
  {
    return $this->suggestedQueryHtml;
  }
  /**
   * @param string
   */
  public function setSuggestionType($suggestionType)
  {
    $this->suggestionType = $suggestionType;
  }
  /**
   * @return string
   */
  public function getSuggestionType()
  {
    return $this->suggestionType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SpellResult::class, 'Google_Service_CloudSearch_SpellResult');
