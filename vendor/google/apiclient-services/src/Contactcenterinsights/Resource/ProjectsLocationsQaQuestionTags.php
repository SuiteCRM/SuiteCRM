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

namespace Google\Service\Contactcenterinsights\Resource;

use Google\Service\Contactcenterinsights\GoogleCloudContactcenterinsightsV1ListQaQuestionTagsResponse;
use Google\Service\Contactcenterinsights\GoogleCloudContactcenterinsightsV1QaQuestionTag;

/**
 * The "qaQuestionTags" collection of methods.
 * Typical usage is:
 *  <code>
 *   $contactcenterinsightsService = new Google\Service\Contactcenterinsights(...);
 *   $qaQuestionTags = $contactcenterinsightsService->projects_locations_qaQuestionTags;
 *  </code>
 */
class ProjectsLocationsQaQuestionTags extends \Google\Service\Resource
{
  /**
   * Create a QaQuestionTag. (qaQuestionTags.create)
   *
   * @param string $parent Required. The parent resource of the QaQuestionTag.
   * @param GoogleCloudContactcenterinsightsV1QaQuestionTag $postBody
   * @param array $optParams Optional parameters.
   * @return GoogleCloudContactcenterinsightsV1QaQuestionTag
   * @throws \Google\Service\Exception
   */
  public function create($parent, GoogleCloudContactcenterinsightsV1QaQuestionTag $postBody, $optParams = [])
  {
    $params = ['parent' => $parent, 'postBody' => $postBody];
    $params = array_merge($params, $optParams);
    return $this->call('create', [$params], GoogleCloudContactcenterinsightsV1QaQuestionTag::class);
  }
  /**
   * Lists the question tags. (qaQuestionTags.listProjectsLocationsQaQuestionTags)
   *
   * @param string $parent Required. The parent resource of the QaQuestionTags.
   * @param array $optParams Optional parameters.
   *
   * @opt_param string filter Optional. A filter to reduce results to a specific
   * subset. Supports disjunctions (OR) and conjunctions (AND). Supported fields
   * include the following: * `project_id` - id of the project to list tags for *
   * `qa_scorecard_revision_id` - id of the scorecard revision to list tags for *
   * `qa_question_id - id of the question to list tags for`
   * @opt_param int pageSize Optional. The maximum number of questions to return
   * in the response. If the value is zero, the service will select a default
   * size. A call might return fewer objects than requested. A non-empty
   * `next_page_token` in the response indicates that more data is available.
   * @opt_param string pageToken Optional. The value returned by the last
   * `ListQaQuestionTagsResponse`. This value indicates that this is a
   * continuation of a prior `ListQaQuestionTags` call and that the system should
   * return the next page of data.
   * @return GoogleCloudContactcenterinsightsV1ListQaQuestionTagsResponse
   * @throws \Google\Service\Exception
   */
  public function listProjectsLocationsQaQuestionTags($parent, $optParams = [])
  {
    $params = ['parent' => $parent];
    $params = array_merge($params, $optParams);
    return $this->call('list', [$params], GoogleCloudContactcenterinsightsV1ListQaQuestionTagsResponse::class);
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ProjectsLocationsQaQuestionTags::class, 'Google_Service_Contactcenterinsights_Resource_ProjectsLocationsQaQuestionTags');
