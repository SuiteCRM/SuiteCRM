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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1SearchEntriesResult extends \Google\Model
{
  protected $dataplexEntryType = GoogleCloudDataplexV1Entry::class;
  protected $dataplexEntryDataType = '';
  /**
   * @var string
   */
  public $linkedResource;
  protected $snippetsType = GoogleCloudDataplexV1SearchEntriesResultSnippets::class;
  protected $snippetsDataType = '';

  /**
   * @param GoogleCloudDataplexV1Entry
   */
  public function setDataplexEntry(GoogleCloudDataplexV1Entry $dataplexEntry)
  {
    $this->dataplexEntry = $dataplexEntry;
  }
  /**
   * @return GoogleCloudDataplexV1Entry
   */
  public function getDataplexEntry()
  {
    return $this->dataplexEntry;
  }
  /**
   * @param string
   */
  public function setLinkedResource($linkedResource)
  {
    $this->linkedResource = $linkedResource;
  }
  /**
   * @return string
   */
  public function getLinkedResource()
  {
    return $this->linkedResource;
  }
  /**
   * @param GoogleCloudDataplexV1SearchEntriesResultSnippets
   */
  public function setSnippets(GoogleCloudDataplexV1SearchEntriesResultSnippets $snippets)
  {
    $this->snippets = $snippets;
  }
  /**
   * @return GoogleCloudDataplexV1SearchEntriesResultSnippets
   */
  public function getSnippets()
  {
    return $this->snippets;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1SearchEntriesResult::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1SearchEntriesResult');
