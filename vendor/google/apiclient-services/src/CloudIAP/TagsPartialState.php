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

namespace Google\Service\CloudIAP;

class TagsPartialState extends \Google\Collection
{
  protected $collection_key = 'tagKeysToRemove';
  /**
   * @var string[]
   */
  public $tagKeysToRemove;
  /**
   * @var string[]
   */
  public $tagsToUpsert;

  /**
   * @param string[]
   */
  public function setTagKeysToRemove($tagKeysToRemove)
  {
    $this->tagKeysToRemove = $tagKeysToRemove;
  }
  /**
   * @return string[]
   */
  public function getTagKeysToRemove()
  {
    return $this->tagKeysToRemove;
  }
  /**
   * @param string[]
   */
  public function setTagsToUpsert($tagsToUpsert)
  {
    $this->tagsToUpsert = $tagsToUpsert;
  }
  /**
   * @return string[]
   */
  public function getTagsToUpsert()
  {
    return $this->tagsToUpsert;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TagsPartialState::class, 'Google_Service_CloudIAP_TagsPartialState');
